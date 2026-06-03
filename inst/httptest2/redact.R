function(x) {
  x <- httptest2::gsub_response(
    x,
    "donnees\\.hydroquebec\\.com/api/explore/v2\\.1/catalog/datasets/donnees-hydrometriques/records",
    "donnees.hydroquebec.com/records"
  )
  x <- httptest2::gsub_response(
    x,
    "www\\.donneesquebec\\.ca/recherche/dataset/[a-f0-9-]+/resource/[a-f0-9-]+/download/",
    "www.donneesquebec.ca/"
  )
  x <- httptest2::gsub_response(
    x,
    "catalogue\\.hakai\\.org/erddap/tabledap/",
    "catalogue.hakai.org/"
  )
  x
}
