\name{countries}
\alias{countries}
\docType{data}
\title{"GeoLite2 Countries List" }
\description{This dataset is used for converting countries abbreviations into full names}
\usage{data("countries")}
\format{
  geoname_id: geoname_id of location

  locale_code: locale_code - factor

  continent_code: continent_code - factor

  continent_name: continent_name - factor

  country_iso_code: country_iso_code - Two letters abbreviation - factor

  country_name Full: country name - factor

}
\source{"http://dev.maxmind.com/geoip/geoip2/geolite2/"}
\examples{
data(countries)
}
\keyword{datasets}
