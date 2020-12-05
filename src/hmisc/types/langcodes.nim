type
  CountryCode* = enum
    ccNone
    ccAfghanistan
    ccÅlandIslands
    ccAlbania
    ccAlgeria
    ccAmericanSamoa
    ccAndorra
    ccAngola
    ccAnguilla
    ccAntarctica
    ccAntiguaandBarbuda
    ccArgentina
    ccArmenia
    ccAruba
    ccAustralia
    ccAustria
    ccAzerbaijan
    ccBahamas
    ccBahrain
    ccBangladesh
    ccBarbados
    ccBelarus
    ccBelgium
    ccBelize
    ccBenin
    ccBermuda
    ccBhutan
    ccBoliviaPlurinationalStateof
    ccBonaireSintEustatiusandSaba
    ccBosniaandHerzegovina
    ccBotswana
    ccBouvetIsland
    ccBrazil
    ccBritishIndianOceanTerritory
    ccBruneiDarussalam
    ccBulgaria
    ccBurkinaFaso
    ccBurundi
    ccCaboVerde
    ccCambodia
    ccCameroon
    ccCanada
    ccCaymanIslands
    ccCentralAfricanRepublic
    ccChad
    ccChile
    ccChina
    ccChristmasIsland
    ccCocosKeelingIslands
    ccColombia
    ccComoros
    ccCongo
    ccCongoDemocraticRepublicofthe
    ccCookIslands
    ccCostaRica
    ccCôtedIvoire
    ccCroatia
    ccCuba
    ccCuraçao
    ccCyprus
    ccCzechia
    ccDenmark
    ccDjibouti
    ccDominica
    ccDominicanRepublic
    ccEcuador
    ccEgypt
    ccElSalvador
    ccEquatorialGuinea
    ccEritrea
    ccEstonia
    ccEswatini
    ccEthiopia
    ccFalklandIslandsMalvinas
    ccFaroeIslands
    ccFiji
    ccFinland
    ccFrance
    ccFrenchGuiana
    ccFrenchPolynesia
    ccFrenchSouthernTerritories
    ccGabon
    ccGambia
    ccGeorgia
    ccGermany
    ccGhana
    ccGibraltar
    ccGreece
    ccGreenland
    ccGrenada
    ccGuadeloupe
    ccGuam
    ccGuatemala
    ccGuernsey
    ccGuinea
    ccGuineaBissau
    ccGuyana
    ccHaiti
    ccHeardIslandandMcDonaldIslands
    ccHolySee
    ccHonduras
    ccHongKong
    ccHungary
    ccIceland
    ccIndia
    ccIndonesia
    ccIranIslamicRepublicof
    ccIraq
    ccIreland
    ccIsleofMan
    ccIsrael
    ccItaly
    ccJamaica
    ccJapan
    ccJersey
    ccJordan
    ccKazakhstan
    ccKenya
    ccKiribati
    ccKoreaDemocraticPeoplesRepublicof
    ccKoreaRepublicof
    ccKuwait
    ccKyrgyzstan
    ccLaoPeoplesDemocraticRepublic
    ccLatvia
    ccLebanon
    ccLesotho
    ccLiberia
    ccLibya
    ccLiechtenstein
    ccLithuania
    ccLuxembourg
    ccMacao
    ccMadagascar
    ccMalawi
    ccMalaysia
    ccMaldives
    ccMali
    ccMalta
    ccMarshallIslands
    ccMartinique
    ccMauritania
    ccMauritius
    ccMayotte
    ccMexico
    ccMicronesiaFederatedStatesof
    ccMoldovaRepublicof
    ccMonaco
    ccMongolia
    ccMontenegro
    ccMontserrat
    ccMorocco
    ccMozambique
    ccMyanmar
    ccNamibia
    ccNauru
    ccNepal
    ccNetherlands
    ccNewCaledonia
    ccNewZealand
    ccNicaragua
    ccNiger
    ccNigeria
    ccNiue
    ccNorfolkIsland
    ccNorthMacedonia
    ccNorthernMarianaIslands
    ccNorway
    ccOman
    ccPakistan
    ccPalau
    ccPalestineStateof
    ccPanama
    ccPapuaNewGuinea
    ccParaguay
    ccPeru
    ccPhilippines
    ccPitcairn
    ccPoland
    ccPortugal
    ccPuertoRico
    ccQatar
    ccRéunion
    ccRomania
    ccRussianFederation
    ccRwanda
    ccSaintBarthélemy
    ccSaintHelenaAscensionandTristandaCunha
    ccSaintKittsandNevis
    ccSaintLucia
    ccSaintMartinFrenchpart
    ccSaintPierreandMiquelon
    ccSaintVincentandtheGrenadines
    ccSamoa
    ccSanMarino
    ccSaoTomeandPrincipe
    ccSaudiArabia
    ccSenegal
    ccSerbia
    ccSeychelles
    ccSierraLeone
    ccSingapore
    ccSintMaartenDutchpart
    ccSlovakia
    ccSlovenia
    ccSolomonIslands
    ccSomalia
    ccSouthAfrica
    ccSouthGeorgiaandtheSouthSandwichIslands
    ccSouthSudan
    ccSpain
    ccSriLanka
    ccSudan
    ccSuriname
    ccSvalbardandJanMayen
    ccSweden
    ccSwitzerland
    ccSyrianArabRepublic
    ccTaiwanProvinceofChina
    ccTajikistan
    ccTanzaniaUnitedRepublicof
    ccThailand
    ccTimorLeste
    ccTogo
    ccTokelau
    ccTonga
    ccTrinidadandTobago
    ccTunisia
    ccTurkey
    ccTurkmenistan
    ccTurksandCaicosIslands
    ccTuvalu
    ccUganda
    ccUkraine
    ccUnitedArabEmirates
    ccUnitedKingdomofGreatBritainandNorthernIreland
    ccUnitedStatesofAmerica
    ccUnitedStatesMinorOutlyingIslands
    ccUruguay
    ccUzbekistan
    ccVanuatu
    ccVenezuelaBolivarianRepublicof
    ccVietNam
    ccVirginIslandsBritish
    ccVirginIslandsUS
    ccWallisandFutuna
    ccWesternSahara
    ccYemen
    ccZambia
    ccZimbabwe


func toTwoLetterCode*(cc: CountryCode): string =
  case cc:
    of ccNone: ""
    of ccAfghanistan: "AF"
    of ccÅlandIslands: "AX"
    of ccAlbania: "AL"
    of ccAlgeria: "DZ"
    of ccAmericanSamoa: "AS"
    of ccAndorra: "AD"
    of ccAngola: "AO"
    of ccAnguilla: "AI"
    of ccAntarctica: "AQ"
    of ccAntiguaandBarbuda: "AG"
    of ccArgentina: "AR"
    of ccArmenia: "AM"
    of ccAruba: "AW"
    of ccAustralia: "AU"
    of ccAustria: "AT"
    of ccAzerbaijan: "AZ"
    of ccBahamas: "BS"
    of ccBahrain: "BH"
    of ccBangladesh: "BD"
    of ccBarbados: "BB"
    of ccBelarus: "BY"
    of ccBelgium: "BE"
    of ccBelize: "BZ"
    of ccBenin: "BJ"
    of ccBermuda: "BM"
    of ccBhutan: "BT"
    of ccBoliviaPlurinationalStateof: "BO"
    of ccBonaireSintEustatiusandSaba: "BQ"
    of ccBosniaandHerzegovina: "BA"
    of ccBotswana: "BW"
    of ccBouvetIsland: "BV"
    of ccBrazil: "BR"
    of ccBritishIndianOceanTerritory: "IO"
    of ccBruneiDarussalam: "BN"
    of ccBulgaria: "BG"
    of ccBurkinaFaso: "BF"
    of ccBurundi: "BI"
    of ccCaboVerde: "CV"
    of ccCambodia: "KH"
    of ccCameroon: "CM"
    of ccCanada: "CA"
    of ccCaymanIslands: "KY"
    of ccCentralAfricanRepublic: "CF"
    of ccChad: "TD"
    of ccChile: "CL"
    of ccChina: "CN"
    of ccChristmasIsland: "CX"
    of ccCocosKeelingIslands: "CC"
    of ccColombia: "CO"
    of ccComoros: "KM"
    of ccCongo: "CG"
    of ccCongoDemocraticRepublicofthe: "CD"
    of ccCookIslands: "CK"
    of ccCostaRica: "CR"
    of ccCôtedIvoire: "CI"
    of ccCroatia: "HR"
    of ccCuba: "CU"
    of ccCuraçao: "CW"
    of ccCyprus: "CY"
    of ccCzechia: "CZ"
    of ccDenmark: "DK"
    of ccDjibouti: "DJ"
    of ccDominica: "DM"
    of ccDominicanRepublic: "DO"
    of ccEcuador: "EC"
    of ccEgypt: "EG"
    of ccElSalvador: "SV"
    of ccEquatorialGuinea: "GQ"
    of ccEritrea: "ER"
    of ccEstonia: "EE"
    of ccEswatini: "SZ"
    of ccEthiopia: "ET"
    of ccFalklandIslandsMalvinas: "FK"
    of ccFaroeIslands: "FO"
    of ccFiji: "FJ"
    of ccFinland: "FI"
    of ccFrance: "FR"
    of ccFrenchGuiana: "GF"
    of ccFrenchPolynesia: "PF"
    of ccFrenchSouthernTerritories: "TF"
    of ccGabon: "GA"
    of ccGambia: "GM"
    of ccGeorgia: "GE"
    of ccGermany: "DE"
    of ccGhana: "GH"
    of ccGibraltar: "GI"
    of ccGreece: "GR"
    of ccGreenland: "GL"
    of ccGrenada: "GD"
    of ccGuadeloupe: "GP"
    of ccGuam: "GU"
    of ccGuatemala: "GT"
    of ccGuernsey: "GG"
    of ccGuinea: "GN"
    of ccGuineaBissau: "GW"
    of ccGuyana: "GY"
    of ccHaiti: "HT"
    of ccHeardIslandandMcDonaldIslands: "HM"
    of ccHolySee: "VA"
    of ccHonduras: "HN"
    of ccHongKong: "HK"
    of ccHungary: "HU"
    of ccIceland: "IS"
    of ccIndia: "IN"
    of ccIndonesia: "ID"
    of ccIranIslamicRepublicof: "IR"
    of ccIraq: "IQ"
    of ccIreland: "IE"
    of ccIsleofMan: "IM"
    of ccIsrael: "IL"
    of ccItaly: "IT"
    of ccJamaica: "JM"
    of ccJapan: "JP"
    of ccJersey: "JE"
    of ccJordan: "JO"
    of ccKazakhstan: "KZ"
    of ccKenya: "KE"
    of ccKiribati: "KI"
    of ccKoreaDemocraticPeoplesRepublicof: "KP"
    of ccKoreaRepublicof: "KR"
    of ccKuwait: "KW"
    of ccKyrgyzstan: "KG"
    of ccLaoPeoplesDemocraticRepublic: "LA"
    of ccLatvia: "LV"
    of ccLebanon: "LB"
    of ccLesotho: "LS"
    of ccLiberia: "LR"
    of ccLibya: "LY"
    of ccLiechtenstein: "LI"
    of ccLithuania: "LT"
    of ccLuxembourg: "LU"
    of ccMacao: "MO"
    of ccMadagascar: "MG"
    of ccMalawi: "MW"
    of ccMalaysia: "MY"
    of ccMaldives: "MV"
    of ccMali: "ML"
    of ccMalta: "MT"
    of ccMarshallIslands: "MH"
    of ccMartinique: "MQ"
    of ccMauritania: "MR"
    of ccMauritius: "MU"
    of ccMayotte: "YT"
    of ccMexico: "MX"
    of ccMicronesiaFederatedStatesof: "FM"
    of ccMoldovaRepublicof: "MD"
    of ccMonaco: "MC"
    of ccMongolia: "MN"
    of ccMontenegro: "ME"
    of ccMontserrat: "MS"
    of ccMorocco: "MA"
    of ccMozambique: "MZ"
    of ccMyanmar: "MM"
    of ccNamibia: "NA"
    of ccNauru: "NR"
    of ccNepal: "NP"
    of ccNetherlands: "NL"
    of ccNewCaledonia: "NC"
    of ccNewZealand: "NZ"
    of ccNicaragua: "NI"
    of ccNiger: "NE"
    of ccNigeria: "NG"
    of ccNiue: "NU"
    of ccNorfolkIsland: "NF"
    of ccNorthMacedonia: "MK"
    of ccNorthernMarianaIslands: "MP"
    of ccNorway: "NO"
    of ccOman: "OM"
    of ccPakistan: "PK"
    of ccPalau: "PW"
    of ccPalestineStateof: "PS"
    of ccPanama: "PA"
    of ccPapuaNewGuinea: "PG"
    of ccParaguay: "PY"
    of ccPeru: "PE"
    of ccPhilippines: "PH"
    of ccPitcairn: "PN"
    of ccPoland: "PL"
    of ccPortugal: "PT"
    of ccPuertoRico: "PR"
    of ccQatar: "QA"
    of ccRéunion: "RE"
    of ccRomania: "RO"
    of ccRussianFederation: "RU"
    of ccRwanda: "RW"
    of ccSaintBarthélemy: "BL"
    of ccSaintHelenaAscensionandTristandaCunha: "SH"
    of ccSaintKittsandNevis: "KN"
    of ccSaintLucia: "LC"
    of ccSaintMartinFrenchpart: "MF"
    of ccSaintPierreandMiquelon: "PM"
    of ccSaintVincentandtheGrenadines: "VC"
    of ccSamoa: "WS"
    of ccSanMarino: "SM"
    of ccSaoTomeandPrincipe: "ST"
    of ccSaudiArabia: "SA"
    of ccSenegal: "SN"
    of ccSerbia: "RS"
    of ccSeychelles: "SC"
    of ccSierraLeone: "SL"
    of ccSingapore: "SG"
    of ccSintMaartenDutchpart: "SX"
    of ccSlovakia: "SK"
    of ccSlovenia: "SI"
    of ccSolomonIslands: "SB"
    of ccSomalia: "SO"
    of ccSouthAfrica: "ZA"
    of ccSouthGeorgiaandtheSouthSandwichIslands: "GS"
    of ccSouthSudan: "SS"
    of ccSpain: "ES"
    of ccSriLanka: "LK"
    of ccSudan: "SD"
    of ccSuriname: "SR"
    of ccSvalbardandJanMayen: "SJ"
    of ccSweden: "SE"
    of ccSwitzerland: "CH"
    of ccSyrianArabRepublic: "SY"
    of ccTaiwanProvinceofChina: "TW"
    of ccTajikistan: "TJ"
    of ccTanzaniaUnitedRepublicof: "TZ"
    of ccThailand: "TH"
    of ccTimorLeste: "TL"
    of ccTogo: "TG"
    of ccTokelau: "TK"
    of ccTonga: "TO"
    of ccTrinidadandTobago: "TT"
    of ccTunisia: "TN"
    of ccTurkey: "TR"
    of ccTurkmenistan: "TM"
    of ccTurksandCaicosIslands: "TC"
    of ccTuvalu: "TV"
    of ccUganda: "UG"
    of ccUkraine: "UA"
    of ccUnitedArabEmirates: "AE"
    of ccUnitedKingdomofGreatBritainandNorthernIreland: "GB"
    of ccUnitedStatesofAmerica: "US"
    of ccUnitedStatesMinorOutlyingIslands: "UM"
    of ccUruguay: "UY"
    of ccUzbekistan: "UZ"
    of ccVanuatu: "VU"
    of ccVenezuelaBolivarianRepublicof: "VE"
    of ccVietNam: "VN"
    of ccVirginIslandsBritish: "VG"
    of ccVirginIslandsUS: "VI"
    of ccWallisandFutuna: "WF"
    of ccWesternSahara: "EH"
    of ccYemen: "YE"
    of ccZambia: "ZM"
    of ccZimbabwe: "ZW"


func toThreeLetterCode*(cc: CountryCode): string =
  case cc:
    of ccNone: ""
    of ccAfghanistan: "AFG"
    of ccÅlandIslands: "ALA"
    of ccAlbania: "ALB"
    of ccAlgeria: "DZA"
    of ccAmericanSamoa: "ASM"
    of ccAndorra: "AND"
    of ccAngola: "AGO"
    of ccAnguilla: "AIA"
    of ccAntarctica: "ATA"
    of ccAntiguaandBarbuda: "ATG"
    of ccArgentina: "ARG"
    of ccArmenia: "ARM"
    of ccAruba: "ABW"
    of ccAustralia: "AUS"
    of ccAustria: "AUT"
    of ccAzerbaijan: "AZE"
    of ccBahamas: "BHS"
    of ccBahrain: "BHR"
    of ccBangladesh: "BGD"
    of ccBarbados: "BRB"
    of ccBelarus: "BLR"
    of ccBelgium: "BEL"
    of ccBelize: "BLZ"
    of ccBenin: "BEN"
    of ccBermuda: "BMU"
    of ccBhutan: "BTN"
    of ccBoliviaPlurinationalStateof: "BOL"
    of ccBonaireSintEustatiusandSaba: "BES"
    of ccBosniaandHerzegovina: "BIH"
    of ccBotswana: "BWA"
    of ccBouvetIsland: "BVT"
    of ccBrazil: "BRA"
    of ccBritishIndianOceanTerritory: "IOT"
    of ccBruneiDarussalam: "BRN"
    of ccBulgaria: "BGR"
    of ccBurkinaFaso: "BFA"
    of ccBurundi: "BDI"
    of ccCaboVerde: "CPV"
    of ccCambodia: "KHM"
    of ccCameroon: "CMR"
    of ccCanada: "CAN"
    of ccCaymanIslands: "CYM"
    of ccCentralAfricanRepublic: "CAF"
    of ccChad: "TCD"
    of ccChile: "CHL"
    of ccChina: "CHN"
    of ccChristmasIsland: "CXR"
    of ccCocosKeelingIslands: "CCK"
    of ccColombia: "COL"
    of ccComoros: "COM"
    of ccCongo: "COG"
    of ccCongoDemocraticRepublicofthe: "COD"
    of ccCookIslands: "COK"
    of ccCostaRica: "CRI"
    of ccCôtedIvoire: "CIV"
    of ccCroatia: "HRV"
    of ccCuba: "CUB"
    of ccCuraçao: "CUW"
    of ccCyprus: "CYP"
    of ccCzechia: "CZE"
    of ccDenmark: "DNK"
    of ccDjibouti: "DJI"
    of ccDominica: "DMA"
    of ccDominicanRepublic: "DOM"
    of ccEcuador: "ECU"
    of ccEgypt: "EGY"
    of ccElSalvador: "SLV"
    of ccEquatorialGuinea: "GNQ"
    of ccEritrea: "ERI"
    of ccEstonia: "EST"
    of ccEswatini: "SWZ"
    of ccEthiopia: "ETH"
    of ccFalklandIslandsMalvinas: "FLK"
    of ccFaroeIslands: "FRO"
    of ccFiji: "FJI"
    of ccFinland: "FIN"
    of ccFrance: "FRA"
    of ccFrenchGuiana: "GUF"
    of ccFrenchPolynesia: "PYF"
    of ccFrenchSouthernTerritories: "ATF"
    of ccGabon: "GAB"
    of ccGambia: "GMB"
    of ccGeorgia: "GEO"
    of ccGermany: "DEU"
    of ccGhana: "GHA"
    of ccGibraltar: "GIB"
    of ccGreece: "GRC"
    of ccGreenland: "GRL"
    of ccGrenada: "GRD"
    of ccGuadeloupe: "GLP"
    of ccGuam: "GUM"
    of ccGuatemala: "GTM"
    of ccGuernsey: "GGY"
    of ccGuinea: "GIN"
    of ccGuineaBissau: "GNB"
    of ccGuyana: "GUY"
    of ccHaiti: "HTI"
    of ccHeardIslandandMcDonaldIslands: "HMD"
    of ccHolySee: "VAT"
    of ccHonduras: "HND"
    of ccHongKong: "HKG"
    of ccHungary: "HUN"
    of ccIceland: "ISL"
    of ccIndia: "IND"
    of ccIndonesia: "IDN"
    of ccIranIslamicRepublicof: "IRN"
    of ccIraq: "IRQ"
    of ccIreland: "IRL"
    of ccIsleofMan: "IMN"
    of ccIsrael: "ISR"
    of ccItaly: "ITA"
    of ccJamaica: "JAM"
    of ccJapan: "JPN"
    of ccJersey: "JEY"
    of ccJordan: "JOR"
    of ccKazakhstan: "KAZ"
    of ccKenya: "KEN"
    of ccKiribati: "KIR"
    of ccKoreaDemocraticPeoplesRepublicof: "PRK"
    of ccKoreaRepublicof: "KOR"
    of ccKuwait: "KWT"
    of ccKyrgyzstan: "KGZ"
    of ccLaoPeoplesDemocraticRepublic: "LAO"
    of ccLatvia: "LVA"
    of ccLebanon: "LBN"
    of ccLesotho: "LSO"
    of ccLiberia: "LBR"
    of ccLibya: "LBY"
    of ccLiechtenstein: "LIE"
    of ccLithuania: "LTU"
    of ccLuxembourg: "LUX"
    of ccMacao: "MAC"
    of ccMadagascar: "MDG"
    of ccMalawi: "MWI"
    of ccMalaysia: "MYS"
    of ccMaldives: "MDV"
    of ccMali: "MLI"
    of ccMalta: "MLT"
    of ccMarshallIslands: "MHL"
    of ccMartinique: "MTQ"
    of ccMauritania: "MRT"
    of ccMauritius: "MUS"
    of ccMayotte: "MYT"
    of ccMexico: "MEX"
    of ccMicronesiaFederatedStatesof: "FSM"
    of ccMoldovaRepublicof: "MDA"
    of ccMonaco: "MCO"
    of ccMongolia: "MNG"
    of ccMontenegro: "MNE"
    of ccMontserrat: "MSR"
    of ccMorocco: "MAR"
    of ccMozambique: "MOZ"
    of ccMyanmar: "MMR"
    of ccNamibia: "NAM"
    of ccNauru: "NRU"
    of ccNepal: "NPL"
    of ccNetherlands: "NLD"
    of ccNewCaledonia: "NCL"
    of ccNewZealand: "NZL"
    of ccNicaragua: "NIC"
    of ccNiger: "NER"
    of ccNigeria: "NGA"
    of ccNiue: "NIU"
    of ccNorfolkIsland: "NFK"
    of ccNorthMacedonia: "MKD"
    of ccNorthernMarianaIslands: "MNP"
    of ccNorway: "NOR"
    of ccOman: "OMN"
    of ccPakistan: "PAK"
    of ccPalau: "PLW"
    of ccPalestineStateof: "PSE"
    of ccPanama: "PAN"
    of ccPapuaNewGuinea: "PNG"
    of ccParaguay: "PRY"
    of ccPeru: "PER"
    of ccPhilippines: "PHL"
    of ccPitcairn: "PCN"
    of ccPoland: "POL"
    of ccPortugal: "PRT"
    of ccPuertoRico: "PRI"
    of ccQatar: "QAT"
    of ccRéunion: "REU"
    of ccRomania: "ROU"
    of ccRussianFederation: "RUS"
    of ccRwanda: "RWA"
    of ccSaintBarthélemy: "BLM"
    of ccSaintHelenaAscensionandTristandaCunha: "SHN"
    of ccSaintKittsandNevis: "KNA"
    of ccSaintLucia: "LCA"
    of ccSaintMartinFrenchpart: "MAF"
    of ccSaintPierreandMiquelon: "SPM"
    of ccSaintVincentandtheGrenadines: "VCT"
    of ccSamoa: "WSM"
    of ccSanMarino: "SMR"
    of ccSaoTomeandPrincipe: "STP"
    of ccSaudiArabia: "SAU"
    of ccSenegal: "SEN"
    of ccSerbia: "SRB"
    of ccSeychelles: "SYC"
    of ccSierraLeone: "SLE"
    of ccSingapore: "SGP"
    of ccSintMaartenDutchpart: "SXM"
    of ccSlovakia: "SVK"
    of ccSlovenia: "SVN"
    of ccSolomonIslands: "SLB"
    of ccSomalia: "SOM"
    of ccSouthAfrica: "ZAF"
    of ccSouthGeorgiaandtheSouthSandwichIslands: "SGS"
    of ccSouthSudan: "SSD"
    of ccSpain: "ESP"
    of ccSriLanka: "LKA"
    of ccSudan: "SDN"
    of ccSuriname: "SUR"
    of ccSvalbardandJanMayen: "SJM"
    of ccSweden: "SWE"
    of ccSwitzerland: "CHE"
    of ccSyrianArabRepublic: "SYR"
    of ccTaiwanProvinceofChina: "TWN"
    of ccTajikistan: "TJK"
    of ccTanzaniaUnitedRepublicof: "TZA"
    of ccThailand: "THA"
    of ccTimorLeste: "TLS"
    of ccTogo: "TGO"
    of ccTokelau: "TKL"
    of ccTonga: "TON"
    of ccTrinidadandTobago: "TTO"
    of ccTunisia: "TUN"
    of ccTurkey: "TUR"
    of ccTurkmenistan: "TKM"
    of ccTurksandCaicosIslands: "TCA"
    of ccTuvalu: "TUV"
    of ccUganda: "UGA"
    of ccUkraine: "UKR"
    of ccUnitedArabEmirates: "ARE"
    of ccUnitedKingdomofGreatBritainandNorthernIreland: "GBR"
    of ccUnitedStatesofAmerica: "USA"
    of ccUnitedStatesMinorOutlyingIslands: "UMI"
    of ccUruguay: "URY"
    of ccUzbekistan: "UZB"
    of ccVanuatu: "VUT"
    of ccVenezuelaBolivarianRepublicof: "VEN"
    of ccVietNam: "VNM"
    of ccVirginIslandsBritish: "VGB"
    of ccVirginIslandsUS: "VIR"
    of ccWallisandFutuna: "WLF"
    of ccWesternSahara: "ESH"
    of ccYemen: "YEM"
    of ccZambia: "ZMB"
    of ccZimbabwe: "ZWE"


func toNumericCode*(cc: CountryCode): string =
  case cc:
    of ccNone: ""
    of ccAfghanistan: "004"
    of ccÅlandIslands: "248"
    of ccAlbania: "008"
    of ccAlgeria: "012"
    of ccAmericanSamoa: "016"
    of ccAndorra: "020"
    of ccAngola: "024"
    of ccAnguilla: "660"
    of ccAntarctica: "010"
    of ccAntiguaandBarbuda: "028"
    of ccArgentina: "032"
    of ccArmenia: "051"
    of ccAruba: "533"
    of ccAustralia: "036"
    of ccAustria: "040"
    of ccAzerbaijan: "031"
    of ccBahamas: "044"
    of ccBahrain: "048"
    of ccBangladesh: "050"
    of ccBarbados: "052"
    of ccBelarus: "112"
    of ccBelgium: "056"
    of ccBelize: "084"
    of ccBenin: "204"
    of ccBermuda: "060"
    of ccBhutan: "064"
    of ccBoliviaPlurinationalStateof: "068"
    of ccBonaireSintEustatiusandSaba: "535"
    of ccBosniaandHerzegovina: "070"
    of ccBotswana: "072"
    of ccBouvetIsland: "074"
    of ccBrazil: "076"
    of ccBritishIndianOceanTerritory: "086"
    of ccBruneiDarussalam: "096"
    of ccBulgaria: "100"
    of ccBurkinaFaso: "854"
    of ccBurundi: "108"
    of ccCaboVerde: "132"
    of ccCambodia: "116"
    of ccCameroon: "120"
    of ccCanada: "124"
    of ccCaymanIslands: "136"
    of ccCentralAfricanRepublic: "140"
    of ccChad: "148"
    of ccChile: "152"
    of ccChina: "156"
    of ccChristmasIsland: "162"
    of ccCocosKeelingIslands: "166"
    of ccColombia: "170"
    of ccComoros: "174"
    of ccCongo: "178"
    of ccCongoDemocraticRepublicofthe: "180"
    of ccCookIslands: "184"
    of ccCostaRica: "188"
    of ccCôtedIvoire: "384"
    of ccCroatia: "191"
    of ccCuba: "192"
    of ccCuraçao: "531"
    of ccCyprus: "196"
    of ccCzechia: "203"
    of ccDenmark: "208"
    of ccDjibouti: "262"
    of ccDominica: "212"
    of ccDominicanRepublic: "214"
    of ccEcuador: "218"
    of ccEgypt: "818"
    of ccElSalvador: "222"
    of ccEquatorialGuinea: "226"
    of ccEritrea: "232"
    of ccEstonia: "233"
    of ccEswatini: "748"
    of ccEthiopia: "231"
    of ccFalklandIslandsMalvinas: "238"
    of ccFaroeIslands: "234"
    of ccFiji: "242"
    of ccFinland: "246"
    of ccFrance: "250"
    of ccFrenchGuiana: "254"
    of ccFrenchPolynesia: "258"
    of ccFrenchSouthernTerritories: "260"
    of ccGabon: "266"
    of ccGambia: "270"
    of ccGeorgia: "268"
    of ccGermany: "276"
    of ccGhana: "288"
    of ccGibraltar: "292"
    of ccGreece: "300"
    of ccGreenland: "304"
    of ccGrenada: "308"
    of ccGuadeloupe: "312"
    of ccGuam: "316"
    of ccGuatemala: "320"
    of ccGuernsey: "831"
    of ccGuinea: "324"
    of ccGuineaBissau: "624"
    of ccGuyana: "328"
    of ccHaiti: "332"
    of ccHeardIslandandMcDonaldIslands: "334"
    of ccHolySee: "336"
    of ccHonduras: "340"
    of ccHongKong: "344"
    of ccHungary: "348"
    of ccIceland: "352"
    of ccIndia: "356"
    of ccIndonesia: "360"
    of ccIranIslamicRepublicof: "364"
    of ccIraq: "368"
    of ccIreland: "372"
    of ccIsleofMan: "833"
    of ccIsrael: "376"
    of ccItaly: "380"
    of ccJamaica: "388"
    of ccJapan: "392"
    of ccJersey: "832"
    of ccJordan: "400"
    of ccKazakhstan: "398"
    of ccKenya: "404"
    of ccKiribati: "296"
    of ccKoreaDemocraticPeoplesRepublicof: "408"
    of ccKoreaRepublicof: "410"
    of ccKuwait: "414"
    of ccKyrgyzstan: "417"
    of ccLaoPeoplesDemocraticRepublic: "418"
    of ccLatvia: "428"
    of ccLebanon: "422"
    of ccLesotho: "426"
    of ccLiberia: "430"
    of ccLibya: "434"
    of ccLiechtenstein: "438"
    of ccLithuania: "440"
    of ccLuxembourg: "442"
    of ccMacao: "446"
    of ccMadagascar: "450"
    of ccMalawi: "454"
    of ccMalaysia: "458"
    of ccMaldives: "462"
    of ccMali: "466"
    of ccMalta: "470"
    of ccMarshallIslands: "584"
    of ccMartinique: "474"
    of ccMauritania: "478"
    of ccMauritius: "480"
    of ccMayotte: "175"
    of ccMexico: "484"
    of ccMicronesiaFederatedStatesof: "583"
    of ccMoldovaRepublicof: "498"
    of ccMonaco: "492"
    of ccMongolia: "496"
    of ccMontenegro: "499"
    of ccMontserrat: "500"
    of ccMorocco: "504"
    of ccMozambique: "508"
    of ccMyanmar: "104"
    of ccNamibia: "516"
    of ccNauru: "520"
    of ccNepal: "524"
    of ccNetherlands: "528"
    of ccNewCaledonia: "540"
    of ccNewZealand: "554"
    of ccNicaragua: "558"
    of ccNiger: "562"
    of ccNigeria: "566"
    of ccNiue: "570"
    of ccNorfolkIsland: "574"
    of ccNorthMacedonia: "807"
    of ccNorthernMarianaIslands: "580"
    of ccNorway: "578"
    of ccOman: "512"
    of ccPakistan: "586"
    of ccPalau: "585"
    of ccPalestineStateof: "275"
    of ccPanama: "591"
    of ccPapuaNewGuinea: "598"
    of ccParaguay: "600"
    of ccPeru: "604"
    of ccPhilippines: "608"
    of ccPitcairn: "612"
    of ccPoland: "616"
    of ccPortugal: "620"
    of ccPuertoRico: "630"
    of ccQatar: "634"
    of ccRéunion: "638"
    of ccRomania: "642"
    of ccRussianFederation: "643"
    of ccRwanda: "646"
    of ccSaintBarthélemy: "652"
    of ccSaintHelenaAscensionandTristandaCunha: "654"
    of ccSaintKittsandNevis: "659"
    of ccSaintLucia: "662"
    of ccSaintMartinFrenchpart: "663"
    of ccSaintPierreandMiquelon: "666"
    of ccSaintVincentandtheGrenadines: "670"
    of ccSamoa: "882"
    of ccSanMarino: "674"
    of ccSaoTomeandPrincipe: "678"
    of ccSaudiArabia: "682"
    of ccSenegal: "686"
    of ccSerbia: "688"
    of ccSeychelles: "690"
    of ccSierraLeone: "694"
    of ccSingapore: "702"
    of ccSintMaartenDutchpart: "534"
    of ccSlovakia: "703"
    of ccSlovenia: "705"
    of ccSolomonIslands: "090"
    of ccSomalia: "706"
    of ccSouthAfrica: "710"
    of ccSouthGeorgiaandtheSouthSandwichIslands: "239"
    of ccSouthSudan: "728"
    of ccSpain: "724"
    of ccSriLanka: "144"
    of ccSudan: "729"
    of ccSuriname: "740"
    of ccSvalbardandJanMayen: "744"
    of ccSweden: "752"
    of ccSwitzerland: "756"
    of ccSyrianArabRepublic: "760"
    of ccTaiwanProvinceofChina: "158"
    of ccTajikistan: "762"
    of ccTanzaniaUnitedRepublicof: "834"
    of ccThailand: "764"
    of ccTimorLeste: "626"
    of ccTogo: "768"
    of ccTokelau: "772"
    of ccTonga: "776"
    of ccTrinidadandTobago: "780"
    of ccTunisia: "788"
    of ccTurkey: "792"
    of ccTurkmenistan: "795"
    of ccTurksandCaicosIslands: "796"
    of ccTuvalu: "798"
    of ccUganda: "800"
    of ccUkraine: "804"
    of ccUnitedArabEmirates: "784"
    of ccUnitedKingdomofGreatBritainandNorthernIreland: "826"
    of ccUnitedStatesofAmerica: "840"
    of ccUnitedStatesMinorOutlyingIslands: "581"
    of ccUruguay: "858"
    of ccUzbekistan: "860"
    of ccVanuatu: "548"
    of ccVenezuelaBolivarianRepublicof: "862"
    of ccVietNam: "704"
    of ccVirginIslandsBritish: "092"
    of ccVirginIslandsUS: "850"
    of ccWallisandFutuna: "876"
    of ccWesternSahara: "732"
    of ccYemen: "887"
    of ccZambia: "894"
    of ccZimbabwe: "716"
type
  LanguageCode* = enum
    lcNone 
    lcAbkhazian ## lcAbkhazian, аҧсуа бызшәа, аҧсшәа
    lcAfar ## lcAfar, Afaraf
    lcAfrikaans ## lcAfrikaans, Afrikaans
    lcAkan ## lcAkan, Akan
    lcAlbanian ## lcAlbanian, Shqip
    lcAmharic ## lcAmharic, አማርኛ
    lcArabic ## lcArabic, العربية
    lcAragonese ## lcAragonese, aragonés
    lcArmenian ## lcArmenian, Հայերեն
    lcAssamese ## lcAssamese, অসমীয়া
    lcAvaric ## lcAvaric, авар мацӀ, магӀарул мацӀ
    lcAvestan ## lcAvestan, avesta
    lcAymara ## lcAymara, aymar aru
    lcAzerbaijani ## lcAzerbaijani, azərbaycan dili
    lcBambara ## lcBambara, bamanankan
    lcBashkir ## lcBashkir, башҡорт теле
    lcBasque ## lcBasque, euskara, euskera
    lcBelarusian ## lcBelarusian, беларуская мова
    lcBengali ## lcBengali, বাংলা
    lcBiharilanguages ## lcBiharilanguages, भोजपुरी
    lcBislama ## lcBislama, Bislama
    lcBosnian ## lcBosnian, bosanski jezik
    lcBreton ## lcBreton, brezhoneg
    lcBulgarian ## lcBulgarian, български език
    lcBurmese ## lcBurmese, ဗမာစာ
    lcCatalanValencian ## lcCatalanValencian, català, valencià
    lcChamorro ## lcChamorro, Chamoru
    lcChechen ## lcChechen, нохчийн мотт
    lcChichewaChewaNyanja ## lcChichewaChewaNyanja, chiCheŵa, chinyanja
    lcChinese ## lcChinese, 中文 (Zhōngwén), 汉语, 漢語
    lcChuvash ## lcChuvash, чӑваш чӗлхи
    lcCornish ## lcCornish, Kernewek
    lcCorsican ## lcCorsican, corsu, lingua corsa
    lcCree ## lcCree, ᓀᐦᐃᔭᐍᐏᐣ
    lcCroatian ## lcCroatian, hrvatski jezik
    lcCzech ## lcCzech, čeština, český jazyk
    lcDanish ## lcDanish, dansk
    lcDivehiDhivehiMaldivian ## lcDivehiDhivehiMaldivian, ދިވެހި
    lcDutchFlemish ## lcDutchFlemish, Nederlands, Vlaams
    lcDzongkha ## lcDzongkha, རྫོང་ཁ
    lcEnglish ## lcEnglish, English
    lcEsperanto ## lcEsperanto, Esperanto
    lcEstonian ## lcEstonian, eesti, eesti keel
    lcEwe ## lcEwe, Eʋegbe
    lcFaroese ## lcFaroese, føroyskt
    lcFijian ## lcFijian, vosa Vakaviti
    lcFinnish ## lcFinnish, suomi, suomen kieli
    lcFrench ## lcFrench, français, langue française
    lcFulah ## lcFulah, Fulfulde, Pulaar, Pular
    lcGalician ## lcGalician, Galego
    lcGeorgian ## lcGeorgian, ქართული
    lcGerman ## lcGerman, Deutsch
    lcGreekModern1453– ## lcGreekModern1453–, ελληνικά
    lcGuarani ## lcGuarani, Avañe'ẽ
    lcGujarati ## lcGujarati, ગુજરાતી
    lcHaitianHaitianCreole ## lcHaitianHaitianCreole, Kreyòl ayisyen
    lcHausa ## lcHausa, (Hausa) هَوُسَ
    lcHebrew ## lcHebrew, עברית
    lcHerero ## lcHerero, Otjiherero
    lcHindi ## lcHindi, हिन्दी, हिंदी
    lcHiriMotu ## lcHiriMotu, Hiri Motu
    lcHungarian ## lcHungarian, magyar
    lcInterlinguaInternationalAuxiliaryLanguageAssociation ## lcInterlinguaInternationalAuxiliaryLanguageAssociation, Interlingua
    lcIndonesian ## lcIndonesian, Bahasa Indonesia
    lcInterlingueOccidental ## lcInterlingueOccidental, (originally:) Occidental, (after WWII:) Interlingue
    lcIrish ## lcIrish, Gaeilge
    lcIgbo ## lcIgbo, Asụsụ Igbo
    lcInupiaq ## lcInupiaq, Iñupiaq, Iñupiatun
    lcIdo ## lcIdo, Ido
    lcIcelandic ## lcIcelandic, Íslenska
    lcItalian ## lcItalian, Italiano
    lcInuktitut ## lcInuktitut, ᐃᓄᒃᑎᑐᑦ
    lcJapanese ## lcJapanese, 日本語 (にほんご)
    lcJavanese ## lcJavanese, ꦧꦱꦗꦮ, Basa Jawa
    lcKalaallisutGreenlandic ## lcKalaallisutGreenlandic, kalaallisut, kalaallit oqaasii
    lcKannada ## lcKannada, ಕನ್ನಡ
    lcKanuri ## lcKanuri, Kanuri
    lcKashmiri ## lcKashmiri, कश्मीरी, كشميري‎
    lcKazakh ## lcKazakh, қазақ тілі
    lcCentralKhmer ## lcCentralKhmer, ខ្មែរ, ខេមរភាសា, ភាសាខ្មែរ
    lcKikuyuGikuyu ## lcKikuyuGikuyu, Gĩkũyũ
    lcKinyarwanda ## lcKinyarwanda, Ikinyarwanda
    lcKirghizKyrgyz ## lcKirghizKyrgyz, Кыргызча, Кыргыз тили
    lcKomi ## lcKomi, коми кыв
    lcKongo ## lcKongo, Kikongo
    lcKorean ## lcKorean, 한국어
    lcKurdish ## lcKurdish, Kurdî, کوردی‎
    lcKuanyamaKwanyama ## lcKuanyamaKwanyama, Kuanyama
    lcLatin ## lcLatin, latine, lingua latina
    lcLuxembourgishLetzeburgesch ## lcLuxembourgishLetzeburgesch, Lëtzebuergesch
    lcGanda ## lcGanda, Luganda
    lcLimburganLimburgerLimburgish ## lcLimburganLimburgerLimburgish, Limburgs
    lcLingala ## lcLingala, Lingála
    lcLao ## lcLao, ພາສາລາວ
    lcLithuanian ## lcLithuanian, lietuvių kalba
    lcLubaKatanga ## lcLubaKatanga, Kiluba
    lcLatvian ## lcLatvian, latviešu valoda
    lcManx ## lcManx, Gaelg, Gailck
    lcMacedonian ## lcMacedonian, македонски јазик
    lcMalagasy ## lcMalagasy, fiteny malagasy
    lcMalay ## lcMalay, Bahasa Melayu, بهاس ملايو‎
    lcMalayalam ## lcMalayalam, മലയാളം
    lcMaltese ## lcMaltese, Malti
    lcMaori ## lcMaori, te reo Māori
    lcMarathi ## lcMarathi, मराठी
    lcMarshallese ## lcMarshallese, Kajin M̧ajeļ
    lcMongolian ## lcMongolian, Монгол хэл
    lcNauru ## lcNauru, Dorerin Naoero
    lcNavajoNavaho ## lcNavajoNavaho, Diné bizaad
    lcNorthNdebele ## lcNorthNdebele, isiNdebele
    lcNepali ## lcNepali, नेपाली
    lcNdonga ## lcNdonga, Owambo
    lcNorwegianBokmål ## lcNorwegianBokmål, Norsk Bokmål
    lcNorwegianNynorsk ## lcNorwegianNynorsk, Norsk Nynorsk
    lcNorwegian ## lcNorwegian, Norsk
    lcSichuanYiNuosu ## lcSichuanYiNuosu, ꆈꌠ꒿ Nuosuhxop
    lcSouthNdebele ## lcSouthNdebele, isiNdebele
    lcOccitan ## lcOccitan, occitan, lenga d'òc
    lcOjibwa ## lcOjibwa, ᐊᓂᔑᓈᐯᒧᐎᓐ
    lcChurchSlavicOldSlavonicChurchSlavonicOldBulgarianOldChurchSlavonic ## lcChurchSlavicOldSlavonicChurchSlavonicOldBulgarianOldChurchSlavonic, ѩзыкъ словѣньскъ
    lcOromo ## lcOromo, Afaan Oromoo
    lcOriya ## lcOriya, ଓଡ଼ିଆ
    lcOssetianOssetic ## lcOssetianOssetic, ирон æвзаг
    lcPunjabiPanjabi ## lcPunjabiPanjabi, ਪੰਜਾਬੀ, پنجابی‎
    lcPali ## lcPali, पालि, पाळि
    lcPersian ## lcPersian, فارسی
    lcPolish ## lcPolish, język polski, polszczyzna
    lcPashtoPushto ## lcPashtoPushto, پښتو
    lcPortuguese ## lcPortuguese, Português
    lcQuechua ## lcQuechua, Runa Simi, Kichwa
    lcRomansh ## lcRomansh, Rumantsch Grischun
    lcRundi ## lcRundi, Ikirundi
    lcRomanianMoldavianMoldovan ## lcRomanianMoldavianMoldovan, Română
    lcRussian ## lcRussian, русский
    lcSanskrit ## lcSanskrit, संस्कृतम्
    lcSardinian ## lcSardinian, sardu
    lcSindhi ## lcSindhi, सिन्धी, سنڌي، سندھی‎
    lcNorthernSami ## lcNorthernSami, Davvisámegiella
    lcSamoan ## lcSamoan, gagana fa'a Samoa
    lcSango ## lcSango, yângâ tî sängö
    lcSerbian ## lcSerbian, српски језик
    lcGaelicScottishGaelic ## lcGaelicScottishGaelic, Gàidhlig
    lcShona ## lcShona, chiShona
    lcSinhalaSinhalese ## lcSinhalaSinhalese, සිංහල
    lcSlovak ## lcSlovak, Slovenčina, Slovenský jazyk
    lcSlovenian ## lcSlovenian, Slovenski jezik, Slovenščina
    lcSomali ## lcSomali, Soomaaliga, af Soomaali
    lcSouthernSotho ## lcSouthernSotho, Sesotho
    lcSpanishCastilian ## lcSpanishCastilian, Español
    lcSundanese ## lcSundanese, Basa Sunda
    lcSwahili ## lcSwahili, Kiswahili
    lcSwati ## lcSwati, SiSwati
    lcSwedish ## lcSwedish, Svenska
    lcTamil ## lcTamil, தமிழ்
    lcTelugu ## lcTelugu, తెలుగు
    lcTajik ## lcTajik, тоҷикӣ, toçikī, تاجیکی‎
    lcThai ## lcThai, ไทย
    lcTigrinya ## lcTigrinya, ትግርኛ
    lcTibetan ## lcTibetan, བོད་ཡིག
    lcTurkmen ## lcTurkmen, Türkmen, Түркмен
    lcTagalog ## lcTagalog, Wikang Tagalog
    lcTswana ## lcTswana, Setswana
    lcTongaTongaIslands ## lcTongaTongaIslands, Faka Tonga
    lcTurkish ## lcTurkish, Türkçe
    lcTsonga ## lcTsonga, Xitsonga
    lcTatar ## lcTatar, татар теле, tatar tele
    lcTwi ## lcTwi, Twi
    lcTahitian ## lcTahitian, Reo Tahiti
    lcUighurUyghur ## lcUighurUyghur, ئۇيغۇرچە‎, Uyghurche
    lcUkrainian ## lcUkrainian, Українська
    lcUrdu ## lcUrdu, اردو
    lcUzbek ## lcUzbek, Oʻzbek, Ўзбек, أۇزبېك‎
    lcVenda ## lcVenda, Tshivenḓa
    lcVietnamese ## lcVietnamese, Tiếng Việt
    lcVolapük ## lcVolapük, Volapük
    lcWalloon ## lcWalloon, Walon
    lcWelsh ## lcWelsh, Cymraeg
    lcWolof ## lcWolof, Wollof
    lcWesternFrisian ## lcWesternFrisian, Frysk
    lcXhosa ## lcXhosa, isiXhosa
    lcYiddish ## lcYiddish, ייִדיש
    lcYoruba ## lcYoruba, Yorùbá
    lcZhuangChuang ## lcZhuangChuang, Saɯ cueŋƅ, Saw cuengh
    lcZulu ## lcZulu, isiZulu


func toTwoLetterCode*(lc: LanguageCode): string =
  case lc:
    of lcNone: ""
    of lcAbkhazian: "ab"
    of lcAfar: "aa"
    of lcAfrikaans: "af"
    of lcAkan: "ak"
    of lcAlbanian: "sq"
    of lcAmharic: "am"
    of lcArabic: "ar"
    of lcAragonese: "an"
    of lcArmenian: "hy"
    of lcAssamese: "as"
    of lcAvaric: "av"
    of lcAvestan: "ae"
    of lcAymara: "ay"
    of lcAzerbaijani: "az"
    of lcBambara: "bm"
    of lcBashkir: "ba"
    of lcBasque: "eu"
    of lcBelarusian: "be"
    of lcBengali: "bn"
    of lcBiharilanguages: "bh"
    of lcBislama: "bi"
    of lcBosnian: "bs"
    of lcBreton: "br"
    of lcBulgarian: "bg"
    of lcBurmese: "my"
    of lcCatalanValencian: "ca"
    of lcChamorro: "ch"
    of lcChechen: "ce"
    of lcChichewaChewaNyanja: "ny"
    of lcChinese: "zh"
    of lcChuvash: "cv"
    of lcCornish: "kw"
    of lcCorsican: "co"
    of lcCree: "cr"
    of lcCroatian: "hr"
    of lcCzech: "cs"
    of lcDanish: "da"
    of lcDivehiDhivehiMaldivian: "dv"
    of lcDutchFlemish: "nl"
    of lcDzongkha: "dz"
    of lcEnglish: "en"
    of lcEsperanto: "eo"
    of lcEstonian: "et"
    of lcEwe: "ee"
    of lcFaroese: "fo"
    of lcFijian: "fj"
    of lcFinnish: "fi"
    of lcFrench: "fr"
    of lcFulah: "ff"
    of lcGalician: "gl"
    of lcGeorgian: "ka"
    of lcGerman: "de"
    of lcGreekModern1453–: "el"
    of lcGuarani: "gn"
    of lcGujarati: "gu"
    of lcHaitianHaitianCreole: "ht"
    of lcHausa: "ha"
    of lcHebrew: "he"
    of lcHerero: "hz"
    of lcHindi: "hi"
    of lcHiriMotu: "ho"
    of lcHungarian: "hu"
    of lcInterlinguaInternationalAuxiliaryLanguageAssociation: "ia"
    of lcIndonesian: "id"
    of lcInterlingueOccidental: "ie"
    of lcIrish: "ga"
    of lcIgbo: "ig"
    of lcInupiaq: "ik"
    of lcIdo: "io"
    of lcIcelandic: "is"
    of lcItalian: "it"
    of lcInuktitut: "iu"
    of lcJapanese: "ja"
    of lcJavanese: "jv"
    of lcKalaallisutGreenlandic: "kl"
    of lcKannada: "kn"
    of lcKanuri: "kr"
    of lcKashmiri: "ks"
    of lcKazakh: "kk"
    of lcCentralKhmer: "km"
    of lcKikuyuGikuyu: "ki"
    of lcKinyarwanda: "rw"
    of lcKirghizKyrgyz: "ky"
    of lcKomi: "kv"
    of lcKongo: "kg"
    of lcKorean: "ko"
    of lcKurdish: "ku"
    of lcKuanyamaKwanyama: "kj"
    of lcLatin: "la"
    of lcLuxembourgishLetzeburgesch: "lb"
    of lcGanda: "lg"
    of lcLimburganLimburgerLimburgish: "li"
    of lcLingala: "ln"
    of lcLao: "lo"
    of lcLithuanian: "lt"
    of lcLubaKatanga: "lu"
    of lcLatvian: "lv"
    of lcManx: "gv"
    of lcMacedonian: "mk"
    of lcMalagasy: "mg"
    of lcMalay: "ms"
    of lcMalayalam: "ml"
    of lcMaltese: "mt"
    of lcMaori: "mi"
    of lcMarathi: "mr"
    of lcMarshallese: "mh"
    of lcMongolian: "mn"
    of lcNauru: "na"
    of lcNavajoNavaho: "nv"
    of lcNorthNdebele: "nd"
    of lcNepali: "ne"
    of lcNdonga: "ng"
    of lcNorwegianBokmål: "nb"
    of lcNorwegianNynorsk: "nn"
    of lcNorwegian: "no"
    of lcSichuanYiNuosu: "ii"
    of lcSouthNdebele: "nr"
    of lcOccitan: "oc"
    of lcOjibwa: "oj"
    of lcChurchSlavicOldSlavonicChurchSlavonicOldBulgarianOldChurchSlavonic: "cu"
    of lcOromo: "om"
    of lcOriya: "or"
    of lcOssetianOssetic: "os"
    of lcPunjabiPanjabi: "pa"
    of lcPali: "pi"
    of lcPersian: "fa"
    of lcPolish: "pl"
    of lcPashtoPushto: "ps"
    of lcPortuguese: "pt"
    of lcQuechua: "qu"
    of lcRomansh: "rm"
    of lcRundi: "rn"
    of lcRomanianMoldavianMoldovan: "ro"
    of lcRussian: "ru"
    of lcSanskrit: "sa"
    of lcSardinian: "sc"
    of lcSindhi: "sd"
    of lcNorthernSami: "se"
    of lcSamoan: "sm"
    of lcSango: "sg"
    of lcSerbian: "sr"
    of lcGaelicScottishGaelic: "gd"
    of lcShona: "sn"
    of lcSinhalaSinhalese: "si"
    of lcSlovak: "sk"
    of lcSlovenian: "sl"
    of lcSomali: "so"
    of lcSouthernSotho: "st"
    of lcSpanishCastilian: "es"
    of lcSundanese: "su"
    of lcSwahili: "sw"
    of lcSwati: "ss"
    of lcSwedish: "sv"
    of lcTamil: "ta"
    of lcTelugu: "te"
    of lcTajik: "tg"
    of lcThai: "th"
    of lcTigrinya: "ti"
    of lcTibetan: "bo"
    of lcTurkmen: "tk"
    of lcTagalog: "tl"
    of lcTswana: "tn"
    of lcTongaTongaIslands: "to"
    of lcTurkish: "tr"
    of lcTsonga: "ts"
    of lcTatar: "tt"
    of lcTwi: "tw"
    of lcTahitian: "ty"
    of lcUighurUyghur: "ug"
    of lcUkrainian: "uk"
    of lcUrdu: "ur"
    of lcUzbek: "uz"
    of lcVenda: "ve"
    of lcVietnamese: "vi"
    of lcVolapük: "vo"
    of lcWalloon: "wa"
    of lcWelsh: "cy"
    of lcWolof: "wo"
    of lcWesternFrisian: "fy"
    of lcXhosa: "xh"
    of lcYiddish: "yi"
    of lcYoruba: "yo"
    of lcZhuangChuang: "za"
    of lcZulu: "zu"


func toThreeLetterCode*(lc: LanguageCode): string =
  case lc:
    of lcNone: ""
    of lcAbkhazian: "abk"
    of lcAfar: "aar"
    of lcAfrikaans: "afr"
    of lcAkan: "aka"
    of lcAlbanian: "sqi"
    of lcAmharic: "amh"
    of lcArabic: "ara"
    of lcAragonese: "arg"
    of lcArmenian: "hye"
    of lcAssamese: "asm"
    of lcAvaric: "ava"
    of lcAvestan: "ave"
    of lcAymara: "aym"
    of lcAzerbaijani: "aze"
    of lcBambara: "bam"
    of lcBashkir: "bak"
    of lcBasque: "eus"
    of lcBelarusian: "bel"
    of lcBengali: "ben"
    of lcBiharilanguages: "bih"
    of lcBislama: "bis"
    of lcBosnian: "bos"
    of lcBreton: "bre"
    of lcBulgarian: "bul"
    of lcBurmese: "mya"
    of lcCatalanValencian: "cat"
    of lcChamorro: "cha"
    of lcChechen: "che"
    of lcChichewaChewaNyanja: "nya"
    of lcChinese: "zho"
    of lcChuvash: "chv"
    of lcCornish: "cor"
    of lcCorsican: "cos"
    of lcCree: "cre"
    of lcCroatian: "hrv"
    of lcCzech: "ces"
    of lcDanish: "dan"
    of lcDivehiDhivehiMaldivian: "div"
    of lcDutchFlemish: "nld"
    of lcDzongkha: "dzo"
    of lcEnglish: "eng"
    of lcEsperanto: "epo"
    of lcEstonian: "est"
    of lcEwe: "ewe"
    of lcFaroese: "fao"
    of lcFijian: "fij"
    of lcFinnish: "fin"
    of lcFrench: "fra"
    of lcFulah: "ful"
    of lcGalician: "glg"
    of lcGeorgian: "kat"
    of lcGerman: "deu"
    of lcGreekModern1453–: "ell"
    of lcGuarani: "grn"
    of lcGujarati: "guj"
    of lcHaitianHaitianCreole: "hat"
    of lcHausa: "hau"
    of lcHebrew: "heb"
    of lcHerero: "her"
    of lcHindi: "hin"
    of lcHiriMotu: "hmo"
    of lcHungarian: "hun"
    of lcInterlinguaInternationalAuxiliaryLanguageAssociation: "ina"
    of lcIndonesian: "ind"
    of lcInterlingueOccidental: "ile"
    of lcIrish: "gle"
    of lcIgbo: "ibo"
    of lcInupiaq: "ipk"
    of lcIdo: "ido"
    of lcIcelandic: "isl"
    of lcItalian: "ita"
    of lcInuktitut: "iku"
    of lcJapanese: "jpn"
    of lcJavanese: "jav"
    of lcKalaallisutGreenlandic: "kal"
    of lcKannada: "kan"
    of lcKanuri: "kau"
    of lcKashmiri: "kas"
    of lcKazakh: "kaz"
    of lcCentralKhmer: "khm"
    of lcKikuyuGikuyu: "kik"
    of lcKinyarwanda: "kin"
    of lcKirghizKyrgyz: "kir"
    of lcKomi: "kom"
    of lcKongo: "kon"
    of lcKorean: "kor"
    of lcKurdish: "kur"
    of lcKuanyamaKwanyama: "kua"
    of lcLatin: "lat"
    of lcLuxembourgishLetzeburgesch: "ltz"
    of lcGanda: "lug"
    of lcLimburganLimburgerLimburgish: "lim"
    of lcLingala: "lin"
    of lcLao: "lao"
    of lcLithuanian: "lit"
    of lcLubaKatanga: "lub"
    of lcLatvian: "lav"
    of lcManx: "glv"
    of lcMacedonian: "mkd"
    of lcMalagasy: "mlg"
    of lcMalay: "msa"
    of lcMalayalam: "mal"
    of lcMaltese: "mlt"
    of lcMaori: "mri"
    of lcMarathi: "mar"
    of lcMarshallese: "mah"
    of lcMongolian: "mon"
    of lcNauru: "nau"
    of lcNavajoNavaho: "nav"
    of lcNorthNdebele: "nde"
    of lcNepali: "nep"
    of lcNdonga: "ndo"
    of lcNorwegianBokmål: "nob"
    of lcNorwegianNynorsk: "nno"
    of lcNorwegian: "nor"
    of lcSichuanYiNuosu: "iii"
    of lcSouthNdebele: "nbl"
    of lcOccitan: "oci"
    of lcOjibwa: "oji"
    of lcChurchSlavicOldSlavonicChurchSlavonicOldBulgarianOldChurchSlavonic: "chu"
    of lcOromo: "orm"
    of lcOriya: "ori"
    of lcOssetianOssetic: "oss"
    of lcPunjabiPanjabi: "pan"
    of lcPali: "pli"
    of lcPersian: "fas"
    of lcPolish: "pol"
    of lcPashtoPushto: "pus"
    of lcPortuguese: "por"
    of lcQuechua: "que"
    of lcRomansh: "roh"
    of lcRundi: "run"
    of lcRomanianMoldavianMoldovan: "ron"
    of lcRussian: "rus"
    of lcSanskrit: "san"
    of lcSardinian: "srd"
    of lcSindhi: "snd"
    of lcNorthernSami: "sme"
    of lcSamoan: "smo"
    of lcSango: "sag"
    of lcSerbian: "srp"
    of lcGaelicScottishGaelic: "gla"
    of lcShona: "sna"
    of lcSinhalaSinhalese: "sin"
    of lcSlovak: "slk"
    of lcSlovenian: "slv"
    of lcSomali: "som"
    of lcSouthernSotho: "sot"
    of lcSpanishCastilian: "spa"
    of lcSundanese: "sun"
    of lcSwahili: "swa"
    of lcSwati: "ssw"
    of lcSwedish: "swe"
    of lcTamil: "tam"
    of lcTelugu: "tel"
    of lcTajik: "tgk"
    of lcThai: "tha"
    of lcTigrinya: "tir"
    of lcTibetan: "bod"
    of lcTurkmen: "tuk"
    of lcTagalog: "tgl"
    of lcTswana: "tsn"
    of lcTongaTongaIslands: "ton"
    of lcTurkish: "tur"
    of lcTsonga: "tso"
    of lcTatar: "tat"
    of lcTwi: "twi"
    of lcTahitian: "tah"
    of lcUighurUyghur: "uig"
    of lcUkrainian: "ukr"
    of lcUrdu: "urd"
    of lcUzbek: "uzb"
    of lcVenda: "ven"
    of lcVietnamese: "vie"
    of lcVolapük: "vol"
    of lcWalloon: "wln"
    of lcWelsh: "cym"
    of lcWolof: "wol"
    of lcWesternFrisian: "fry"
    of lcXhosa: "xho"
    of lcYiddish: "yid"
    of lcYoruba: "yor"
    of lcZhuangChuang: "zha"
    of lcZulu: "zul"
