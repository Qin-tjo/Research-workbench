"""OncoLit command line — the key-free halves of the pipeline.

Two subcommands, designed to bracket the analysis the Claude Code session does:

    python -m app.cli search "<query>" [--years 2022-2026] [--sources a,b] [--max N] -o run.json
    # ... the session reads run.json and writes analysis.json ...
    python -m app.cli render run.json analysis.json -o report.html

Neither step needs an API key. `search` hits only free public sources; `render`
just templates the session's analysis into the existing HTML report.
"""

from __future__ import annotations

import argparse
import asyncio
import json
import sys
from pathlib import Path
from typing import List, Optional, Tuple

from app.core.config import get_settings
from app.models.article import Article, RankedArticle, SearchFilters
from app.pipeline.analysis import (
    Analysis,
    ArticleSummary,
    SynthesisResult,
    article_key,
    based_on,
)
from app.pipeline.search import corpus_coverage, run_search, split_tiers
from app.report.renderer import render_report

# --- helpers -------------------------------------------------------------------------

def _parse_years(value: Optional[str]) -> Tuple[Optional[int], Optional[int]]:
    if not value:
        return None, None
    value = value.strip()
    if "-" in value:
        lo, _, hi = value.partition("-")
        return (int(lo) if lo else None, int(hi) if hi else None)
    year = int(value)
    return year, year


def _ranked_payload(r: RankedArticle, idx: Optional[int] = None) -> dict:
    payload = {"score": r.score, "article": r.article.model_dump(mode="json")}
    if idx is not None:
        payload["id"] = idx
    return payload


# --- search --------------------------------------------------------------------------

async def _search_and_enrich(query, filters, key_count):
    """Search, tier, and pull OA full text for the key tier (mutates in place)."""
    from app.pipeline.fulltext import enrich_fulltext

    ranked, warnings = await run_search(query, filters)
    key, additional = split_tiers(ranked, key_count)
    upgraded = await enrich_fulltext([r.article for r in key]) if key else 0
    return ranked, key, additional, warnings, upgraded


def cmd_search(args: argparse.Namespace) -> int:
    from app.adapters import registry

    settings = get_settings()
    year_from, year_to = _parse_years(args.years)
    sources = (
        [s.strip() for s in args.sources.split(",") if s.strip()]
        if args.sources
        else registry.default_source_names()
    )
    filters = SearchFilters(
        sources=sources,
        year_from=year_from,
        year_to=year_to,
        max_results_per_source=args.max or settings.default_max_results,
        include_preprints=not args.no_preprints,
        prioritize_citations=not args.no_citation_priority,
    )

    ranked, key, additional, warnings, upgraded = asyncio.run(
        _search_and_enrich(args.query, filters, settings.key_paper_count)
    )
    coverage = corpus_coverage(ranked, len(key))

    run = {
        "query": args.query,
        "sources": sources,
        "filters": filters.model_dump(),
        "coverage": coverage,
        "warnings": warnings,
        "key_papers": [_ranked_payload(r, i) for i, r in enumerate(key, start=1)],
        "additional": [
            _ranked_payload(r) for r in additional[: settings.max_additional_display]
        ],
        "additional_total": len(additional),
    }
    Path(args.output).write_text(json.dumps(run, indent=2), "utf-8")

    print(f"Wrote {args.output}")
    print(
        f"  {coverage['total']} results | {len(key)} key papers "
        f"({upgraded} with full text) | {len(additional)} additional"
    )
    if warnings:
        print("  warnings:")
        for w in warnings:
            print(f"    - {w}")
    if not key:
        print("  NOTE: no abstract-bearing papers found — analysis will be sparse.")
    return 0


# --- render --------------------------------------------------------------------------

def cmd_render(args: argparse.Namespace) -> int:
    run = json.loads(Path(args.run).read_text("utf-8"))
    analysis = Analysis.model_validate_json(Path(args.analysis).read_text("utf-8"))

    by_id = analysis.by_id()
    key_papers: List[RankedArticle] = []
    summaries: List[ArticleSummary] = []
    for entry in run.get("key_papers", []):
        art = Article.model_validate(entry["article"])
        key_papers.append(RankedArticle(article=art, score=entry.get("score", 0.0)))
        aa = by_id.get(entry["id"])
        summaries.append(
            ArticleSummary(
                article_id=article_key(art),
                tldr=(aa.tldr if aa and aa.tldr else (art.abstract or art.title or "")),
                based_on=based_on(art.content_level),
            )
        )

    additional = [
        RankedArticle(
            article=Article.model_validate(e["article"]), score=e.get("score", 0.0)
        )
        for e in run.get("additional", [])
    ]

    html = render_report(
        run.get("query", ""),
        key_papers,
        summaries,
        analysis.synthesis or SynthesisResult(),
        table=analysis.table,
        additional=additional,
        additional_total=run.get("additional_total", len(additional)),
        sources=run.get("sources", []),
        coverage=run.get("coverage", {}),
        warnings=run.get("warnings", []),
        model_note="analysis by Claude Code session (no external API)",
    )

    out = Path(args.output)
    out.parent.mkdir(parents=True, exist_ok=True)
    out.write_text(html, "utf-8")
    print(f"Wrote {out} ({len(html):,} bytes)")
    return 0


# --- ask: ephemeral HTML input form --------------------------------------------------

_FORM_HTML = """<!DOCTYPE html><html><head><meta charset="utf-8">
<title>OncoLit — new search</title>
<style>
 body{font:16px/1.6 -apple-system,Segoe UI,Roboto,sans-serif;max-width:640px;margin:6vh auto;
   padding:0 24px;color:#111827}
 h1{font-size:1.4rem;margin:0 0 4px} p.sub{color:#6b7280;margin:0 0 24px;font-size:.9rem}
 label{display:block;font-weight:600;margin:16px 0 6px;font-size:.9rem}
 textarea,input[type=text],input[type=number]{width:100%;padding:10px 12px;border:1px solid #e5e7eb;
   border-radius:8px;font:inherit}
 textarea{min-height:90px;resize:vertical}
 .row{display:flex;gap:16px}.row>div{flex:1}
 .check{display:flex;align-items:center;gap:8px;margin-top:18px;font-size:.9rem}
 .check input{width:auto}
 button{margin-top:24px;background:#2563eb;color:#fff;border:0;border-radius:8px;padding:12px 22px;
   font:inherit;font-weight:600;cursor:pointer}
 .hint{color:#6b7280;font-size:.78rem;font-weight:400}
</style></head><body>
<h1>OncoLit literature search</h1>
<p class="sub">Describe your research question. Claude Code will search, read the papers, and
build a report.</p>
<form method="POST" action="/">
 <label>Research question</label>
 <textarea name="query" required
   placeholder="e.g. acquired resistance to KRAS G12C inhibitors in NSCLC"></textarea>
 <div class="row">
   <div><label>Year range <span class="hint">(optional)</span></label>
     <input type="text" name="years" placeholder="2022-2026"></div>
   <div><label>Max per source</label>
     <input type="number" name="max" value="20" min="1" max="100"></div>
 </div>
 <label>Limit to sources <span class="hint">(optional, comma-separated; blank = all)</span></label>
 <input type="text" name="sources" placeholder="pubmed, europepmc, asco">
 <label class="check"><input type="checkbox" name="prioritize_citations" checked>
   Give well-cited papers more weight in selection</label>
 <button type="submit">Generate report</button>
</form></body></html>"""

_DONE_HTML = """<!DOCTYPE html><html><head><meta charset="utf-8"><title>OncoLit</title>
<style>body{font:16px/1.6 -apple-system,sans-serif;max-width:560px;margin:12vh auto;
 padding:0 24px;color:#111827;text-align:center}</style></head><body>
<h1>Got it ✓</h1><p>Your question was captured. You can close this tab — Claude Code is now
searching and building your report.</p></body></html>"""


def cmd_ask(args: argparse.Namespace) -> int:
    import webbrowser
    from http.server import BaseHTTPRequestHandler, HTTPServer
    from urllib.parse import parse_qs

    class Handler(BaseHTTPRequestHandler):
        def log_message(self, *a):  # silence default logging
            pass

        def do_GET(self):
            self.send_response(200)
            self.send_header("Content-Type", "text/html; charset=utf-8")
            self.end_headers()
            self.wfile.write(_FORM_HTML.encode("utf-8"))

        def do_POST(self):
            length = int(self.headers.get("Content-Length", 0))
            form = parse_qs(self.rfile.read(length).decode("utf-8"))
            self.server.result = {
                "query": (form.get("query", [""])[0]).strip(),
                "years": (form.get("years", [""])[0]).strip(),
                "max": (form.get("max", [""])[0]).strip(),
                "sources": (form.get("sources", [""])[0]).strip(),
                "prioritize_citations": "prioritize_citations" in form,
            }
            self.send_response(200)
            self.send_header("Content-Type", "text/html; charset=utf-8")
            self.end_headers()
            self.wfile.write(_DONE_HTML.encode("utf-8"))

    server = HTTPServer(("127.0.0.1", 0), Handler)
    server.result = None
    url = f"http://127.0.0.1:{server.server_port}/"
    print(f"Opening input form at {url} … (fill it in and click Generate)")
    webbrowser.open(url)
    while server.result is None:  # handle GET(s) until one POST arrives
        server.handle_request()
    server.server_close()

    if not server.result["query"]:
        print("No question entered.")
        return 1
    Path(args.output).write_text(json.dumps(server.result, indent=2), "utf-8")
    print(f"Wrote {args.output}: {server.result['query'][:80]}")
    return 0


# --- entrypoint ----------------------------------------------------------------------

def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(prog="oncolit", description=__doc__)
    sub = p.add_subparsers(dest="command", required=True)

    a = sub.add_parser("ask", help="open an HTML form to enter the question -> request.json")
    a.add_argument("-o", "--output", default="request.json")
    a.set_defaults(func=cmd_ask)

    s = sub.add_parser("search", help="search sources -> run.json (no API key)")
    s.add_argument("query")
    s.add_argument("--years", help="year range, e.g. 2022-2026 or a single year")
    s.add_argument(
        "--sources", help="comma-separated source names (default: all but semanticscholar)"
    )
    s.add_argument("--max", type=int, help="max results per source")
    s.add_argument("--no-preprints", action="store_true")
    s.add_argument("--no-citation-priority", action="store_true",
                   help="don't extra-weight well-cited papers")
    s.add_argument("-o", "--output", default="run.json")
    s.set_defaults(func=cmd_search)

    r = sub.add_parser("render", help="run.json + analysis.json -> HTML report")
    r.add_argument("run")
    r.add_argument("analysis")
    r.add_argument("-o", "--output", required=True)
    r.set_defaults(func=cmd_render)

    return p


def main(argv: Optional[List[str]] = None) -> int:
    args = build_parser().parse_args(argv)
    return args.func(args)


if __name__ == "__main__":
    sys.exit(main())
