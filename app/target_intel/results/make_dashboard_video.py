"""Capture a section-by-section scrolling video of the target_intel dashboard
and convert it to a LinkedIn-friendly 1080p MP4."""
import asyncio
import os
import subprocess
from pathlib import Path

import imageio_ffmpeg
from playwright.async_api import async_playwright

HERE = Path(__file__).parent
DASHBOARD = HERE / "target_intel_dashboard.html"
OUT_DIR = HERE / "_video_capture"
FINAL_MP4 = HERE / "target_intel_dashboard.mp4"

WIDTH, HEIGHT = 1440, 810

# sections to traverse, top -> bottom. The video scrolls THROUGH each section
# so every line is on-screen at some point, with a short pause between sections.
SECTIONS = [
    "genomic-landscape",
    "mechanism",
    "clinical",
    "synthesis",
]
# scroll speed (pixels per second) — lower = slower / easier to read
SCROLL_PX_PER_SEC = 140
# extra dwell at each section's TOP and BOTTOM
DWELL_TOP_SEC = 3.5
DWELL_BOTTOM_SEC = 2.5
# pause at the very top before starting
INTRO_SEC = 3.5
# pause at the very bottom after finishing
OUTRO_SEC = 2.5

SCROLL_JS = """
async ({targetY, durationMs}) => {
  const startY = window.scrollY;
  const delta = targetY - startY;
  const t0 = performance.now();
  return await new Promise(resolve => {
    function step(now) {
      const p = Math.min(1, (now - t0) / durationMs);
      // easeInOutCubic
      const e = p < 0.5 ? 4*p*p*p : 1 - Math.pow(-2*p + 2, 3)/2;
      window.scrollTo(0, startY + delta * e);
      if (p < 1) requestAnimationFrame(step);
      else resolve(true);
    }
    requestAnimationFrame(step);
  });
}
"""

async def main():
    OUT_DIR.mkdir(exist_ok=True)
    async with async_playwright() as pw:
        browser = await pw.chromium.launch(headless=True)
        context = await browser.new_context(
            viewport={"width": WIDTH, "height": HEIGHT},
            record_video_dir=str(OUT_DIR),
            record_video_size={"width": WIDTH, "height": HEIGHT},
            device_scale_factor=1,
        )
        page = await context.new_page()
        await page.goto(DASHBOARD.as_uri(), wait_until="networkidle")
        # let fonts/images settle
        await page.wait_for_timeout(1500)

        doc_height = await page.evaluate("document.body.scrollHeight")
        view_h = await page.evaluate("window.innerHeight")
        print(f"doc height: {doc_height}px, viewport: {view_h}px")

        # compute (top_y, bottom_y) for each section
        ranges = []
        for sid in SECTIONS:
            top_y = await page.evaluate(
                "(id) => document.getElementById(id).getBoundingClientRect().top + window.scrollY",
                sid,
            )
            ranges.append((sid, top_y))
        # bottom of last section = doc end; bottoms = next section's top
        section_bounds = []
        for i, (sid, top_y) in enumerate(ranges):
            bot_y = ranges[i+1][1] if i+1 < len(ranges) else doc_height
            # we want the bottom of the section to be visible, so scroll target is bot_y - view_h
            scroll_top = max(0, top_y - 10)
            scroll_bot = max(scroll_top, bot_y - view_h)
            section_bounds.append((sid, scroll_top, scroll_bot))
            print(f"  {sid}: scroll y {scroll_top:.0f} -> {scroll_bot:.0f}  (height {bot_y-top_y:.0f}px)")

        # intro at top
        await page.evaluate("window.scrollTo(0, 0)")
        await page.wait_for_timeout(int(INTRO_SEC * 1000))

        for sid, top, bot in section_bounds:
            # snap to section top
            await page.evaluate(SCROLL_JS, {"targetY": top, "durationMs": 900})
            await page.wait_for_timeout(int(DWELL_TOP_SEC * 1000))
            # scroll through to bottom at constant speed
            distance = bot - top
            dur_ms = max(700, int(1000 * distance / SCROLL_PX_PER_SEC))
            print(f"-> {sid}: scrolling {distance:.0f}px over {dur_ms}ms")
            await page.evaluate(SCROLL_JS, {"targetY": bot, "durationMs": dur_ms})
            await page.wait_for_timeout(int(DWELL_BOTTOM_SEC * 1000))

        # outro at bottom
        await page.evaluate(SCROLL_JS, {"targetY": doc_height, "durationMs": 800})
        await page.wait_for_timeout(int(OUTRO_SEC * 1000))

        await context.close()
        await browser.close()

    # find the webm just produced
    webms = sorted(OUT_DIR.glob("*.webm"), key=lambda p: p.stat().st_mtime)
    assert webms, "no webm captured"
    webm = webms[-1]
    print(f"captured: {webm}")

    ffmpeg = imageio_ffmpeg.get_ffmpeg_exe()
    cmd = [
        ffmpeg, "-y", "-i", str(webm),
        "-c:v", "libx264", "-preset", "medium", "-crf", "20",
        "-pix_fmt", "yuv420p",
        "-movflags", "+faststart",
        str(FINAL_MP4),
    ]
    print(" ".join(cmd))
    subprocess.run(cmd, check=True)
    print(f"\nWROTE {FINAL_MP4}  ({FINAL_MP4.stat().st_size/1e6:.1f} MB)")

if __name__ == "__main__":
    asyncio.run(main())
