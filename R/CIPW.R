#' CIPW Norm
#' @description \code{CIPW()} calculates the mineral composition of an igneous rock sample, based on ther major oxides and weight percent
#' @param data A dataframe (tibble) with the major oxides in the first column and their percent weight in the secon column
#'
#' @return Summar table for the CIPW norm, with all the minerals and their percent weight, as well as separate tables for the ferromagentic minerals and its components
#' @export
#' @importFrom ggplot2 .data
#' @details The names of the oxides must be: SiO2, TiO2, Al2O3, Fe2O3, FeO, MnO, MgO, CaO, Na2O, K2O, P2O5. If any of the major oxides is not present just use 0 for their percent weight
#' @examples
#' d = data.frame(O = c('SiO2', 'TiO2', 'Al2O3', 'Fe2O3', 'FeO',
#'                      'MnO', 'MgO', 'CaO', 'Na2O', 'K2O', 'P2O5'),
#'                W = c(35.6,5.98,12.9,7.68,9.28,.05,
#'                      5.4,8.46,8.35,2.78,2.13))
#' CIPW(d)
#'
CIPW = function(data) {

  # Major oxides and their molecular weights
  cipw.mw = tibble::tibble(
    Oxide = c("SiO2","TiO2","Al2O3","Fe2O3","FeO",
              "MnO","MgO","CaO","Na2O","K2O","P2O5"),
    MW = c(60.08,79.90,101.96,159.69,71.85,
           70.94,40.30,56.08,61.98,94.20,141.94)
  )

  # Norm minerals and their molecular weights
  cipw.min = tibble::tibble(
    Mineral = c('Quartz','Corundum','Zircon','Orthoclase','Albite',
                'Anorthite','Leucite','Nepheline','Kaliophilite',
                'Halite','Thenardite','Sodium-Carbonate','Acmite',
                'Sodium-metasilicate','Potassium-metasilicate',
                'Diopside','Wollastonite','Hypersthene','Olivine',
                'Calcium-silicate','Magnetite','Chromite','Ilmenite',
                'Hematite','Titanite','Perovskite','Rutile','Apatite',
                'Fluorite','Pyrite','Calcite'),
    Abbr = c('q','c','z','or','ab','an','lc','ne','kp','hl','th','nc',
             'ac','ns','ks','di','wo','hy','ol','cs','mt','cm','il',
             'hm','tn','pf','ru','ap','fl','pr','cc'),
    MW = c(60.08,101.96,183.31,556.64,524.42,278.20,436.48,
           284.10,316.32,132.89,142.04,105.99,461.99,122.06,
           154.28,1.00,116.16,1.00,1.00,172.24,231.54,223.84,
           151.75,159.69,196.06,135.98,79.90,336.21,78.08,119.98,100.09)
  )

  # tibble to store results
  mynorm = rep(0,nrow(cipw.min)) %>%
    purrr::set_names(cipw.min$Abbr) %>%
    tibble::as_tibble_row()

  # function to calculate results for di, hy, ol
  dihyol = function(mynorm) {
    mgdi = M*mynorm$di
    fedi = F*mynorm$di
    di = mgdi*216.55 + fedi*248.09
    mynorm$di = di

    en = M*mynorm$hy
    fs = F*mynorm$hy
    hy = en*100.39 + fs*131.93
    mynorm$hy = hy

    fo = M*mynorm$ol
    fa = F*mynorm$ol
    ol = fo*140.7 + fa*203.78
    mynorm$ol = ol

    parts = tibble::tibble(
      Mineral = rep(c('Diopside','Hypersthene','Olivine'),each=2),
      Abbr = rep(c('di','hy','ol'),each=2),
      Part = c('mgdi','fedi','en','fs','fo','fa'),
      MF = rep(c('M','F'),3),
      MF_prop = rep(c(M,F),3),
      MW = c(216.55,248.09,100.39,131.93,140.7,203.78),
      Result = c(mgdi,fedi,en,fs,fo,fa)/.data$MW,
      Perc_W = c(mgdi,fedi,en,fs,fo,fa)
    )

    return(list(mynorm = mynorm,
                parts = parts))
  }

  # data prep
  dat = data %>%
    purrr::set_names(c('Oxide','Perc_W'))

  dat = dat %>%
    dplyr::left_join(cipw.mw, by = 'Oxide') %>%
    dplyr::mutate(mol.prop = .data$Perc_W/.data$MW)

  # molar proportions
  mol.prop = dat$mol.prop %>%
    purrr::set_names(dat$Oxide)

  # MnO & FeO
  FeO = sum(mol.prop[c('FeO','MnO')])

  # ap
  ap = mol.prop['P2O5']
  mynorm$ap = ap
  CaO = mol.prop['CaO'] - 10/3*ap

  # FeO vs TiO2
  if (FeO > mol.prop['TiO2']) {
    il = mol.prop['TiO2']
    mynorm$il = il
    FeO = FeO - il
    TiO2 = 0
  } else {
    il = FeO
    mynorm$il = il
    TiO2 = mol.prop['TiO2'] - il
    FeO = 0
  }

  # Al203 vs K2O
  if (mol.prop['Al2O3'] > mol.prop['K2O']) {
    or.prime = mol.prop['K2O']
    mynorm$or = or.prime
    Al2O3 = mol.prop['Al2O3'] - or.prime
    Si = 6 * or.prime
    ks = 0
    mynorm$ks = ks
    K2O = 0
  } else {
    or.prime = mol.prop['Al2O3']
    mynorm$or = or.prime
    K2O = mol.prop['K2O'] - or.prime
    ks = K2O
    mynorm$ks = ks
    Si = 6 * or.prime + ks
    Al2O3 = 0
  }

  # Al203 vs Na2O
  if (Al2O3 > mol.prop['Na2O']) {
    ab.prime = mol.prop['Na2O']
    mynorm$ab = ab.prime
    Al2O3 = Al2O3 - ab.prime
    Si = Si + 6 * ab.prime
    Na2O = 0
  } else {
    ab.prime = Al2O3
    mynorm$ab = ab.prime
    Na2O = mol.prop['Na2O'] - ab.prime
    Si = Si + 6 * ab.prime
    Al2O3 = 0
  }

  # Na20 vs Fe2O3
  if (Na2O > mol.prop['Fe2O3']) {
    ac = mol.prop['Fe2O3']
    mynorm$ac = ac
    Na2O = Na2O - ac
    ns = Na2O
    mynorm$ns = ns
    Si = Si + 4 * ac + ns
    Fe2O3 = 0
  } else {
    ac = Na2O
    mynorm$ac = ac
    ns = 0
    mynorm$ns = ns
    Fe2O3 = mol.prop['Fe2O3'] - ac
    Si = Si + 4 * ac
    Na2O = 0
  }

  # Al203 vs CaO
  if (Al2O3 > CaO) {
    an = CaO
    mynorm$an = an
    Al2O3 = Al2O3 - an
    CaO = 0
    c = Al2O3
    mynorm$c = c
    Si = Si + 2 * an
    Al2O3 = 0
  } else {
    an = Al2O3
    mynorm$an = an
    c = 0
    mynorm$c = c
    CaO = CaO - an
    Si = Si + 2 * an
  }

  # Ca0 vs TiO2
  if (CaO > TiO2) {
    tn.prime = TiO2
    mynorm$tn = tn.prime
    CaO = CaO - tn.prime
    ru = 0
    mynorm$ru = ru
    Si = Si + tn.prime
  } else {
    tn.prime = CaO
    mynorm$tn = tn.prime
    TiO2 = TiO2 - tn.prime
    ru = TiO2
    mynorm$ru = ru
    Si = Si + tn.prime
    TiO2 = 0
  }

  # Fe2O3 vs FeO
  if (Fe2O3 > FeO) {
    mt = FeO
    mynorm$mt = mt
    Fe2O3 = Fe2O3 - mt
    hm = Fe2O3
    mynorm$hm = hm
    FeO = 0
  } else {
    mt = Fe2O3
    mynorm$mt = mt
    hm = 0
    mynorm$hm = hm
    FeO = FeO - mt
    Fe2O3 = 0
  }

  # F & M
  MgFeO = (mol.prop['MgO'] + FeO)
  M = (mol.prop['MgO'] / MgFeO)
  F = (FeO / MgFeO)

  # CaO vs (Mg,Fe)O
  if (CaO > MgFeO) {
    di.prime = MgFeO
    mynorm$di = di.prime
    CaO = CaO - di.prime
    wo.prime = CaO
    mynorm$wo = wo.prime
    hy.prime = 0
    mynorm$hy = hy.prime
    Si = Si + 2 * di.prime + wo.prime
    CaO = 0
  } else {
    di.prime = CaO
    mynorm$di = di.prime
    MgFeO = MgFeO - di.prime
    hy.prime = MgFeO
    mynorm$hy = hy.prime
    Si = Si + 2 * di.prime + hy.prime
    wo.prime = 0
    mynorm$wo = wo.prime
  }

  # Si & D
  if (mol.prop['SiO2'] > Si) {
    q = mol.prop['SiO2'] - Si
    mynorm$q = q

    mynorm = dihyol(mynorm)$mynorm
    parts = dihyol(mynorm)$parts

    D = Si - mol.prop['SiO2']

  } else {
    D = Si - mol.prop['SiO2']
    D0 = D

    if (D < hy.prime/2) {
      ol = D
      mynorm$ol = ol
      hy = hy.prime - 2 * D
      mynorm$hy = hy
      D = 0

      mynorm = dihyol(mynorm)$mynorm
      parts = dihyol(mynorm)$parts

    } else {
      ol = hy.prime/2
      mynorm$ol = ol
      hy = 0
      mynorm$hy = hy
      D1 = D - hy.prime/2

      if (D1 < tn.prime) {
        tn = tn.prime - D1
        mynorm$tn = tn
        pf = D1
        mynorm$pf = pf
        D1 = 0

        mynorm = dihyol(mynorm)$mynorm
        parts = dihyol(mynorm)$parts

      } else {
        pf = tn.prime
        mynorm$pf = pf
        tn = 0
        mynorm$tn = tn
        D2 = D1 - tn.prime

        if (D2 < 4*ab.prime) {
          ne = D2/4
          mynorm$ne = ne
          ab = ab.prime - D2/4
          mynorm$ab = ab
          D2 = 0

          mynorm = dihyol(mynorm)$mynorm
          parts = dihyol(mynorm)$parts

        } else {
          ne.prime = ab.prime
          mynorm$ne = ne.prime
          ab = 0
          mynorm$ab = ab
          D3 = D2 - 4*ab.prime

          if (D3 < 2*or.prime) {
            lc = D3/2
            mynorm$lc = lc
            or = or.prime - D3/2
            mynorm$or = or
            D3 = 0

            mynorm = dihyol(mynorm)$mynorm
            parts = dihyol(mynorm)$parts

          } else {
            lc.prime = or.prime
            mynorm$lc = lc.prime
            or = 0
            mynorm$or = or
            D4 = D3 - 2*or.prime

            if (D4 < wo.prime/2) {
              cs = D4
              mynorm$cs = cs
              wo = wo.prime - 2*D4
              mynorm$wo = wo
              D4 = 0

              mynorm = dihyol(mynorm)$mynorm
              parts = dihyol(mynorm)$parts

            } else {
              cs = wo.prime/2
              mynorm$cs = cs
              wo = 0
              mynorm$wo = wo
              D5 = D4 - wo.prime/2

              if (D5 < di.prime) {
                cs = cs + D5/2
                mynorm$cs = cs
                ol = ol + D5/2
                mynorm$ol = ol
                di = di.prime - D5
                mynorm$di = di
                D5 = 0

                mynorm = dihyol(mynorm)$mynorm
                parts = dihyol(mynorm)$parts

              } else {
                cs = cs + di.prime/2
                mynorm$cs = cs
                ol = ol + di.prime/2
                mynorm$ol = ol
                di = 0
                mynorm$di = di
                D6 = D5 - di.prime
                kp = D6/2
                mynorm$kp = kp
                lc = lc.prime - D6/2
                mynorm$lc = lc

                mynorm = dihyol(mynorm)$mynorm
                parts = dihyol(mynorm)$parts

                if (lc < 0) {
                  lc4 = 0
                  kp = or.prime
                  mynorm$lc = lc4
                  mynorm$kp = kp
                } else {
                  lc4 = or.prime
                  kp = kp
                  mynorm$lc = lc4
                }

                if (lc > 0 & kp > 0) {
                  lc5 = lc
                  mynorm$lc = lc5
                } else {
                  lc5 = lc4
                  mynorm$lc = lc5
                }
              }
            }
          }
        }
      }
    }
  }

  CIPW = mynorm %>%
    tidyr::pivot_longer(dplyr::everything(),
                        names_to = 'Abbr',
                        values_to = 'Result') %>%
    dplyr::left_join(cipw.min, by = 'Abbr') %>%
    dplyr::relocate(.data$Mineral) %>%
    dplyr::mutate(Perc_W = (.data$Result * .data$MW) %>% round(2),
                  Result = .data$Result %>% round(4)) %>%
    dplyr::filter(.data$Perc_W > 0)

  parts.summary = parts %>%
    dplyr::group_by(.data$Mineral) %>%
    dplyr::summarise(MW = sum(.data$MW * .data$MF_prop),
                     Result = sum(.data$Result),
                     Perc_W = sum(.data$Perc_W))

  return(list(CIPW = CIPW,
              parts = parts,
              parts.summary = parts.summary))

}
