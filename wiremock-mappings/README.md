# wiremock mappings

The JSON files here support mocking the IFSC REST API using [`wiremock`].
Because the entities modeled here have relationships to one another,
certain IDs need to match up for the service to make any sense. Those are (with json paths given in `jq` format):

- [`landingPage.json`]: `.leagues[0].id` must match the `league` query parameter in [`seasonLeagueResults.json`] `.request.url`
- [`seasonLeagueResults.json`]: the event id at the end of the `url` in `.events[0].url`  (ugh, I _know_) must match the `event_id` query parameter in [`eventResults.json`] `.request.url`
- [`eventResults.json`]: the `full_results_url` value in `d_cats[0].full_results_url` must match the `result_url` query parameter in [`eventFullResultsMen.json`] `.request.url`, and the `full_results_url` value in `d_cats[1].full_results_url` must match the `result_url` query parameter in [`eventFullResultsWomen.json`] `.request.url`

Is this all a bit fiddly (especially the string parsing to get event IDs out)? Absolutely! But hey man, I don't think this API was set up for me in particular, so 🤷🏻‍♂️.

[`wiremock`]: https://wiremock.org/
[`landingPage.json`]: ./landingPage.json
[`seasonLeagueResults.json`]: ./seasonLeagueResults.json
[`eventResults.json`]: ./eventResults.json
[`eventFullResultsMen.json`]: ./eventFullResultsMen.json
[`eventFullResultsWomen.json`]: ./eventFullResultsWomen.json
