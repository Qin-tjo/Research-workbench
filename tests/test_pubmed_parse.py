"""PubMed XML parsing against a recorded fixture (no network)."""

from __future__ import annotations

from app.adapters.pubmed import PubMedAdapter
from app.models.article import ContentLevel

SAMPLE_XML = """<?xml version="1.0"?>
<PubmedArticleSet>
 <PubmedArticle>
  <MedlineCitation>
   <PMID>40000001</PMID>
   <Article>
    <Journal><Title>Journal of Clinical Oncology</Title>
     <JournalIssue><PubDate><Year>2025</Year></PubDate></JournalIssue></Journal>
    <ArticleTitle>KRAS G12C inhibition in NSCLC</ArticleTitle>
    <Abstract>
      <AbstractText Label="BACKGROUND">Resistance emerges.</AbstractText>
      <AbstractText Label="RESULTS">ORR was 37%.</AbstractText>
    </Abstract>
    <AuthorList>
      <Author><LastName>Smith</LastName><ForeName>Jane</ForeName></Author>
    </AuthorList>
    <PublicationTypeList><PublicationType>Journal Article</PublicationType></PublicationTypeList>
   </Article>
  </MedlineCitation>
  <PubmedData><ArticleIdList>
    <ArticleId IdType="doi">10.1200/JCO.2025.001</ArticleId>
  </ArticleIdList></PubmedData>
 </PubmedArticle>
 <PubmedArticle>
  <MedlineCitation>
   <PMID>40000002</PMID>
   <Article>
    <ArticleTitle>Conference abstract with no abstract body</ArticleTitle>
    <PublicationTypeList><PublicationType>Meeting Abstract</PublicationType></PublicationTypeList>
   </Article>
  </MedlineCitation>
 </PubmedArticle>
</PubmedArticleSet>"""


def test_parse_extracts_fields_and_handles_missing_abstract():
    adapter = PubMedAdapter()
    arts = adapter._parse(SAMPLE_XML)
    assert len(arts) == 2

    first = arts[0]
    assert first.pmid == "40000001"
    assert first.doi == "10.1200/JCO.2025.001"
    assert first.year == 2025
    assert first.venue == "Journal of Clinical Oncology"
    assert "ORR was 37%" in first.abstract
    assert first.content_level == ContentLevel.ABSTRACT_ONLY
    assert first.url.endswith("/40000001/")
    assert first.authors[0].name == "Jane Smith"

    second = arts[1]
    assert second.abstract is None
    assert second.content_level == ContentLevel.TITLE_ONLY
    assert second.is_conference is True
