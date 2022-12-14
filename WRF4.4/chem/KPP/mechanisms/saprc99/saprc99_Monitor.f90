! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! Utility Data Module File
! 
! Generated by KPP-2.1 symbolic chemistry Kinetics PreProcessor
!       (http://www.cs.vt.edu/~asandu/Software/KPP)
! KPP is distributed under GPL, the general public licence
!       (http://www.gnu.org/copyleft/gpl.html)
! (C) 1995-1997, V. Damian & A. Sandu, CGRER, Univ. Iowa
! (C) 1997-2005, A. Sandu, Michigan Tech, Virginia Tech
!     With important contributions from:
!        M. Damian, Villanova University, USA
!        R. Sander, Max-Planck Institute for Chemistry, Mainz, Germany
! 
! File                 : saprc99_Monitor.f90
! Time                 : Mon Sep 26 23:37:53 2022
! Working directory    : /home/ess/xiaodong/WORK/WRFCHEM/model/WRF4.4/chem/KPP/mechanisms/saprc99
! Equation file        : saprc99.kpp
! Output root filename : saprc99
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



MODULE saprc99_Monitor


  CHARACTER(LEN=12), PARAMETER, DIMENSION(88) :: SPC_NAMES = (/ &
     'H2SO4       ','HCOOH       ','CCO_OH      ', &
     'RCO_OH      ','ISOPOOH2    ','PSD1        ', &
     'IEPOX       ','ISOP2O2     ','CO2         ', &
     'CCO_OOH     ','RCO_OOH     ','XN          ', &
     'XC          ','O1D         ','CH4         ', &
     'SO2         ','ISOPOOH     ','ISOPOO      ', &
     'C2H6        ','PAN         ','PAN2        ', &
     'PBZN        ','MA_PAN      ','H2O2        ', &
     'ETOH        ','C3H8        ','BACL        ', &
     'N2O5        ','HONO        ','ALK3        ', &
     'TBU_O       ','ALK5        ','MEOH        ', &
     'ARO2        ','COOH        ','HOCOO       ', &
     'BZNO2_O     ','HNO4        ','ALK4        ', &
     'ARO1        ','DCB3        ','DCB2        ', &
     'CRES        ','C2H2        ','DCB1        ', &
     'NPHE        ','BALD        ','ROOH        ', &
     'PHEN        ','MGLY        ','CO          ', &
     'HNO3        ','ETHENE      ','ACET        ', &
     'C3H6        ','GLY         ','BZ_O        ', &
     'ISOPRENE    ','TERP        ','R2O2        ', &
     'SESQ        ','METHACRO    ','OLE1        ', &
     'ISOPROD     ','OLE2        ','MVK         ', &
     'CCHO        ','HCHO        ','RNO3        ', &
     'O3P         ','RCHO        ','MEK         ', &
     'PROD2       ','O3          ','MA_RCO3     ', &
     'CCO_O2      ','RO2_R       ','NO          ', &
     'NO2         ','NO3         ','C_O2        ', &
     'OH          ','RCO_O2      ','BZCO_O2     ', &
     'HO2         ','RO2_N       ','H2O         ', &
     'M           ' /)

  INTEGER, DIMENSION(1) :: LOOKAT
  INTEGER, DIMENSION(1) :: MONITOR
  CHARACTER(LEN=12), DIMENSION(1) :: SMASS
  CHARACTER(LEN=100), PARAMETER, DIMENSION(30) :: EQN_NAMES_0 = (/ &
     '                  NO2 --> O3P + NO                                                                  ', &
     '              O3P + M --> O3                                                                        ', &
     '             O3P + O3 --> M                                                                         ', &
     '         O3P + NO + M --> NO2                                                                       ', &
     '            O3P + NO2 --> NO                                                                        ', &
     '            O3P + NO2 --> NO3                                                                       ', &
     '              O3 + NO --> NO2                                                                       ', &
     '             O3 + NO2 --> NO3                                                                       ', &
     '             NO + NO3 --> 6.95262e-31 NO2                                                           ', &
     '   6.95262e-31 NO + M --> 6.95262e-31 NO2                                                           ', &
     '            NO2 + NO3 --> N2O5                                                                      ', &
     '                 N2O5 --> NO2 + NO3                                                                 ', &
     '           N2O5 + H2O --> 6.95262e-31 HNO3                                                          ', &
     '            NO2 + NO3 --> NO + NO2                                                                  ', &
     '                  NO3 --> NO                                                                        ', &
     '                  NO3 --> O3P + NO2                                                                 ', &
     '                   O3 --> O3P                                                                       ', &
     '                   O3 --> O1D                                                                       ', &
     '            O1D + H2O --> 6.95262e-31 OH                                                            ', &
     '              O1D + M --> O3P                                                                       ', &
     '              NO + OH --> HONO                                                                      ', &
     '                 HONO --> NO + OH                                                                   ', &
     '                 HONO --> NO2 + HO2                                                                 ', &
     '            HONO + OH --> NO2                                                                       ', &
     '             NO2 + OH --> HNO3                                                                      ', &
     '             NO3 + OH --> NO2 + HO2                                                                 ', &
     '            HNO3 + OH --> NO3                                                                       ', &
     '                 HNO3 --> NO2 + OH                                                                  ', &
     '              CO + OH --> HO2                                                                       ', &
     '              O3 + OH --> HO2                                                                       ' /)
  CHARACTER(LEN=100), PARAMETER, DIMENSION(30) :: EQN_NAMES_1 = (/ &
     '             NO + HO2 --> NO2 + OH                                                                  ', &
     '            NO2 + HO2 --> HNO4                                                                      ', &
     '                 HNO4 --> NO2 + HO2                                                                 ', &
     '                 HNO4 --> 6.95262e-31 NO2 + 6.95262e-31 NO3 + 6.95262e-31 OH + 6.95262e-31 HO2      ', &
     '            HNO4 + OH --> NO2                                                                       ', &
     '             O3 + HO2 --> OH                                                                        ', &
     '      6.95262e-31 HO2 --> H2O2                                                                      ', &
     '6.95262e-31 HO2 + H2O --> H2O2                                                                      ', &
     '            NO3 + HO2 --> 6.95262e-31 HNO3 + 6.95262e-31 NO2 + 6.95262e-31 OH                       ', &
     '      6.95262e-31 NO3 --> 6.95262e-31 NO2                                                           ', &
     '                 H2O2 --> 6.95262e-31 OH                                                            ', &
     '            H2O2 + OH --> HO2                                                                       ', &
     '             OH + HO2 --> H2O + M                                                                   ', &
     '             SO2 + OH --> H2SO4 + HO2                                                               ', &
     '               OH + M --> HO2                                                                       ', &
     '            NO + C_O2 --> HCHO + NO2 + HO2                                                          ', &
     '           C_O2 + HO2 --> COOH                                                                      ', &
     '           NO3 + C_O2 --> HCHO + NO2 + HO2                                                          ', &
     '     6.95262e-31 C_O2 --> MEOH + HCHO                                                               ', &
     '     6.95262e-31 C_O2 --> 6.95262e-31 HCHO + 6.95262e-31 HO2                                        ', &
     '           RO2_R + NO --> NO2 + HO2                                                                 ', &
     '          RO2_R + HO2 --> ROOH                                                                      ', &
     '          RO2_R + NO3 --> NO2 + HO2                                                                 ', &
     '         RO2_R + C_O2 --> 6.95262e-31 MEOH + 6.95262e-31 HCHO + HO2                                 ', &
     '    6.95262e-31 RO2_R --> HO2                                                                       ', &
     '            R2O2 + NO --> NO2                                                                       ', &
     '           R2O2 + HO2 --> HO2                                                                       ', &
     '           R2O2 + NO3 --> NO2                                                                       ', &
     '          R2O2 + C_O2 --> C_O2                                                                      ', &
     '         R2O2 + RO2_R --> RO2_R                                                                     ' /)
  CHARACTER(LEN=100), PARAMETER, DIMENSION(30) :: EQN_NAMES_2 = (/ &
     '     6.95262e-31 R2O2 --> 6.95262e-31 R2O2                                                          ', &
     '           NO + RO2_N --> RNO3                                                                      ', &
     '          HO2 + RO2_N --> ROOH                                                                      ', &
     '         C_O2 + RO2_N --> 6.95262e-31 MEOH + 6.95262e-31 HCHO + 6.95262e-31 MEK + 6.95262e-31 PROD2 ', &
     '          NO3 + RO2_N --> MEK + NO2 + HO2                                                           ', &
     '        RO2_R + RO2_N --> 6.95262e-31 MEK + 6.95262e-31 PROD2 + HO2                                 ', &
     '         R2O2 + RO2_N --> RO2_N                                                                     ', &
     '    6.95262e-31 RO2_N --> MEK + PROD2 + HO2                                                         ', &
     '         CCO_O2 + NO2 --> PAN                                                                       ', &
     '                  PAN --> CCO_O2 + NO2                                                              ', &
     '          CCO_O2 + NO --> NO2 + C_O2                                                                ', &
     '         CCO_O2 + HO2 --> 6.95262e-31 CCO_OH + 6.95262e-31 CCO_OOH + 6.95262e-31 O3                 ', &
     '         CCO_O2 + NO3 --> NO2 + C_O2                                                                ', &
     '        CCO_O2 + C_O2 --> CCO_OH + HCHO                                                             ', &
     '       CCO_O2 + RO2_R --> CCO_OH                                                                    ', &
     '        R2O2 + CCO_O2 --> CCO_O2                                                                    ', &
     '       CCO_O2 + RO2_N --> CCO_OH + PROD2                                                            ', &
     '   6.95262e-31 CCO_O2 --> 6.95262e-31 C_O2                                                          ', &
     '         NO2 + RCO_O2 --> PAN2                                                                      ', &
     '                 PAN2 --> NO2 + RCO_O2                                                              ', &
     '          NO + RCO_O2 --> CCHO + RO2_R + NO2                                                        ', &
     '         RCO_O2 + HO2 --> 6.95262e-31 RCO_OH + 6.95262e-31 RCO_OOH + 6.95262e-31 O3                 ', &
     '         NO3 + RCO_O2 --> CCHO + RO2_R + NO2                                                        ', &
     '        C_O2 + RCO_O2 --> RCO_OH + HCHO                                                             ', &
     '       RO2_R + RCO_O2 --> RCO_OH                                                                    ', &
     '        R2O2 + RCO_O2 --> RCO_O2                                                                    ', &
     '       RCO_O2 + RO2_N --> RCO_OH + PROD2                                                            ', &
     '      CCO_O2 + RCO_O2 --> CCHO + RO2_R + C_O2                                                       ', &
     '   6.95262e-31 RCO_O2 --> 6.95262e-31 CCHO + 6.95262e-31 RO2_R                                      ', &
     '        NO2 + BZCO_O2 --> PBZN                                                                      ' /)
  CHARACTER(LEN=100), PARAMETER, DIMENSION(30) :: EQN_NAMES_3 = (/ &
     '                 PBZN --> NO2 + BZCO_O2                                                             ', &
     '         NO + BZCO_O2 --> BZ_O + R2O2 + NO2                                                         ', &
     '        BZCO_O2 + HO2 --> 6.95262e-31 RCO_OH + 6.95262e-31 RCO_OOH + 6.95262e-31 O3                 ', &
     '        NO3 + BZCO_O2 --> BZ_O + R2O2 + NO2                                                         ', &
     '       C_O2 + BZCO_O2 --> RCO_OH + HCHO                                                             ', &
     '      RO2_R + BZCO_O2 --> RCO_OH                                                                    ', &
     '       R2O2 + BZCO_O2 --> BZCO_O2                                                                   ', &
     '      BZCO_O2 + RO2_N --> RCO_OH + PROD2                                                            ', &
     '     CCO_O2 + BZCO_O2 --> BZ_O + R2O2 + C_O2                                                        ', &
     '     RCO_O2 + BZCO_O2 --> BZ_O + R2O2 + CCHO + RO2_R                                                ', &
     '  6.95262e-31 BZCO_O2 --> 6.95262e-31 BZ_O + 6.95262e-31 R2O2                                       ', &
     '        MA_RCO3 + NO2 --> MA_PAN                                                                    ', &
     '               MA_PAN --> MA_RCO3 + NO2                                                             ', &
     '         MA_RCO3 + NO --> HCHO + CCO_O2 + NO2                                                       ', &
     '        MA_RCO3 + HO2 --> 6.95262e-31 RCO_OH + 6.95262e-31 RCO_OOH + 6.95262e-31 O3                 ', &
     '        MA_RCO3 + NO3 --> HCHO + CCO_O2 + NO2                                                       ', &
     '       MA_RCO3 + C_O2 --> RCO_OH + HCHO                                                             ', &
     '      MA_RCO3 + RO2_R --> RCO_OH                                                                    ', &
     '       R2O2 + MA_RCO3 --> MA_RCO3                                                                   ', &
     '      MA_RCO3 + RO2_N --> 6.95262e-31 RCO_OH                                                        ', &
     '     MA_RCO3 + CCO_O2 --> HCHO + CCO_O2 + C_O2                                                      ', &
     '     MA_RCO3 + RCO_O2 --> CCHO + HCHO + CCO_O2 + RO2_R                                              ', &
     '    MA_RCO3 + BZCO_O2 --> BZ_O + R2O2 + HCHO + CCO_O2                                               ', &
     '  6.95262e-31 MA_RCO3 --> 6.95262e-31 HCHO + 6.95262e-31 CCO_O2                                     ', &
     '          TBU_O + NO2 --> RNO3                                                                      ', &
     '                TBU_O --> ACET + C_O2                                                               ', &
     '           BZ_O + NO2 --> NPHE                                                                      ', &
     '           BZ_O + HO2 --> PHEN                                                                      ', &
     '                 BZ_O --> PHEN                                                                      ', &
     '        BZNO2_O + NO2 --> 6.95262e-31 XN + 6.95262e-31 XC                                           ' /)
  CHARACTER(LEN=100), PARAMETER, DIMENSION(30) :: EQN_NAMES_4 = (/ &
     '        BZNO2_O + HO2 --> NPHE                                                                      ', &
     '              BZNO2_O --> NPHE                                                                      ', &
     '                 HCHO --> CO + 6.95262e-31 HO2                                                      ', &
     '                 HCHO --> CO                                                                        ', &
     '            HCHO + OH --> CO + HO2                                                                  ', &
     '           HCHO + HO2 --> HOCOO                                                                     ', &
     '                HOCOO --> HCHO + HO2                                                                ', &
     '           HOCOO + NO --> HCOOH + NO2 + HO2                                                         ', &
     '           HCHO + NO3 --> CO + HNO3 + HO2                                                           ', &
     '            CCHO + OH --> CCO_O2                                                                    ', &
     '                 CCHO --> CO + C_O2 + HO2                                                           ', &
     '           CCHO + NO3 --> HNO3 + CCO_O2                                                             ', &
     '            RCHO + OH --> 6.95262e-31 CO + 6.95262e-31 CCHO + 6.95262e-31 RO2_R + 6.95262e-31 RCO_O2', &
     '                 RCHO --> CO + CCHO + RO2_R + HO2                                                   ', &
     '           RCHO + NO3 --> HNO3 + RCO_O2                                                             ', &
     '            ACET + OH --> R2O2 + HCHO + CCO_O2                                                      ', &
     '                 ACET --> CCO_O2 + C_O2                                                             ', &
     '             MEK + OH --> 6.95262e-31 R2O2 + 6.95262e-31 CCHO + 6.95262e-31 HCHO + 6.95262e-31 RCHO ', &
     '                  MEK --> CCHO + CCO_O2 + RO2_R                                                     ', &
     '            MEOH + OH --> HCHO + HO2                                                                ', &
     '            ETOH + OH --> 6.95262e-31 CCHO + 6.95262e-31 HCHO + 6.95262e-31 RO2_R + 6.95262e-31 HO2 ', &
     '            COOH + OH --> 6.95262e-31 HCHO + 6.95262e-31 C_O2 + 6.95262e-31 OH                      ', &
     '                 COOH --> HCHO + OH + HO2                                                           ', &
     '            ROOH + OH --> RCHO + 6.95262e-31 RO2_R + 6.95262e-31 OH                                 ', &
     '                 ROOH --> RCHO + OH + HO2                                                           ', &
     '                  GLY --> 6.95262e-31 CO + 6.95262e-31 HO2                                          ', &
     '                  GLY --> CO + HCHO                                                                 ', &
     '             GLY + OH --> 6.95262e-31 CO + 6.95262e-31 RCO_O2 + 6.95262e-31 HO2                     ', &
     '            GLY + NO3 --> 6.95262e-31 CO + HNO3 + 6.95262e-31 RCO_O2 + 6.95262e-31 HO2              ', &
     '                 MGLY --> CO + CCO_O2 + HO2                                                         ' /)
  CHARACTER(LEN=100), PARAMETER, DIMENSION(30) :: EQN_NAMES_5 = (/ &
     '            MGLY + OH --> CO + CCO_O2                                                               ', &
     '           MGLY + NO3 --> CO + HNO3 + CCO_O2                                                        ', &
     '                 BACL --> 6.95262e-31 CCO_O2                                                        ', &
     '            PHEN + OH --> 6.95262e-31 GLY + 6.95262e-31 BZ_O + 6.95262e-31 RO2_R                    ', &
     '           PHEN + NO3 --> HNO3 + BZ_O                                                               ', &
     '            CRES + OH --> 6.95262e-31 MGLY + 6.95262e-31 BZ_O + 6.95262e-31 RO2_R                   ', &
     '           CRES + NO3 --> HNO3 + BZ_O                                                               ', &
     '           NPHE + NO3 --> BZNO2_O + HNO3                                                            ', &
     '            BALD + OH --> BZCO_O2                                                                   ', &
     '                 BALD --> 6.95262e-31 XC                                                            ', &
     '           BALD + NO3 --> HNO3 + BZCO_O2                                                            ', &
     '        METHACRO + OH --> 6.95262e-31 MGLY + 6.95262e-31 CO + 6.95262e-31 HCHO + 6.95262e-31 MEK + 6', &
     '        METHACRO + O3 --> 6.95262e-31 HCOOH + 6.95262e-31 MGLY + 6.95262e-31 CO + 6.95262e-31 HCHO +', &
     '       METHACRO + NO3 --> 6.95262e-31 CO + 6.95262e-31 HNO3 + 6.95262e-31 MA_RCO3 + 6.95262e-31 RO2_', &
     '       METHACRO + O3P --> RCHO                                                                      ', &
     '             METHACRO --> 6.95262e-31 CO + 6.95262e-31 HCHO + 6.95262e-31 MA_RCO3 + 6.95262e-31 CCO_', &
     '             MVK + OH --> 6.95262e-31 MGLY + 6.95262e-31 R2O2 + 6.95262e-31 HCHO + 6.95262e-31 RCHO ', &
     '             MVK + O3 --> 6.95262e-31 HCOOH + 6.95262e-31 MGLY + 6.95262e-31 CO + 6.95262e-31 HCHO +', &
     '            MVK + O3P --> 6.95262e-31 RCHO + 6.95262e-31 MEK                                        ', &
     '                  MVK --> 6.95262e-31 CO + 6.95262e-31 PROD2 + 6.95262e-31 MA_RCO3 + 6.95262e-31 C_O', &
     '         ISOPROD + OH --> 6.95262e-31 MGLY + 6.95262e-31 CO + 6.95262e-31 GLY + 6.95262e-31 CCHO + 6', &
     '         ISOPROD + O3 --> 6.95262e-31 HCOOH + 6.95262e-31 RCO_OH + 6.95262e-31 MGLY + 6.95262e-31 CO', &
     '        ISOPROD + NO3 --> 6.95262e-31 MGLY + 6.95262e-31 CO + 6.95262e-31 HNO3 + 6.95262e-31 HCHO + ', &
     '              ISOPROD --> 6.95262e-31 CO + 6.95262e-31 CCHO + 6.95262e-31 HCHO + 6.95262e-31 MEK + 6', &
     '           PROD2 + OH --> 6.95262e-31 CCHO + 6.95262e-31 HCHO + 6.95262e-31 RCHO + 6.95262e-31 MEK +', &
     '                PROD2 --> 6.95262e-31 R2O2 + 6.95262e-31 CCHO + 6.95262e-31 HCHO + 6.95262e-31 RCHO ', &
     '            RNO3 + OH --> 6.95262e-31 ACET + 6.95262e-31 R2O2 + 6.95262e-31 CCHO + 6.95262e-31 HCHO ', &
     '                 RNO3 --> 6.95262e-31 ACET + 6.95262e-31 R2O2 + 6.95262e-31 CCHO + 6.95262e-31 HCHO ', &
     '            DCB1 + OH --> CO + RCHO + RO2_R                                                         ', &
     '            DCB1 + O3 --> 6.95262e-31 CO + GLY + 6.95262e-31 OH + 6.95262e-31 HO2                   ' /)
  CHARACTER(LEN=100), PARAMETER, DIMENSION(30) :: EQN_NAMES_6 = (/ &
     '            DCB2 + OH --> R2O2 + RCHO + CCO_O2                                                      ', &
     '                 DCB2 --> 6.95262e-31 MGLY + CO + 6.95262e-31 GLY + R2O2 + 6.95262e-31 CCO_O2 + RO2_', &
     '            DCB3 + OH --> R2O2 + RCHO + CCO_O2                                                      ', &
     '                 DCB3 --> 6.95262e-31 MGLY + CO + 6.95262e-31 GLY + R2O2 + 6.95262e-31 CCO_O2 + RO2_', &
     '             CH4 + OH --> C_O2                                                                      ', &
     '          ETHENE + OH --> 6.95262e-31 CCHO + 6.95262e-31 HCHO + RO2_R                               ', &
     '          ETHENE + O3 --> 6.95262e-31 HCOOH + 6.95262e-31 CO + HCHO + 6.95262e-31 OH + 6.95262e-31 H', &
     '         ETHENE + NO3 --> RCHO + RO2_R                                                              ', &
     '         ETHENE + O3P --> 6.95262e-31 CO + 6.95262e-31 GLY + 6.95262e-31 CCHO + 6.95262e-31 HCHO + 6', &
     '        ISOPRENE + OH --> ISOPOO + 6.95262e-31 R2O2 + 6.95262e-31 METHACRO + 6.95262e-31 ISOPROD + 6', &
     '         ISOPOO + HO2 --> 6.95262e-31 ISOPOOH + HO2                                                 ', &
     '          ISOPOO + NO --> NO                                                                        ', &
     '   6.95262e-31 ISOPOO --> PSD1                                                                      ', &
     '         ISOPOOH + OH --> 6.95262e-31 IEPOX + 6.95262e-31 ISOP2O2 + 6.95262e-31 ISOPOO + OH         ', &
     '           IEPOX + OH --> PSD1 + OH                                                                 ', &
     '        ISOP2O2 + HO2 --> ISOPOOH2 + HO2                                                            ', &
     '         ISOP2O2 + NO --> NO                                                                        ', &
     '              ISOP2O2 --> PSD1                                                                      ', &
     '        ISOPRENE + O3 --> 6.95262e-31 HCOOH + 6.95262e-31 RCO_OH + 6.95262e-31 CO + 6.95262e-31 R2O2', &
     '       ISOPRENE + NO3 --> 6.95262e-31 R2O2 + 6.95262e-31 ISOPROD + 6.95262e-31 RO2_R + 6.95262e-31 N', &
     '       ISOPRENE + O3P --> 6.95262e-31 R2O2 + 6.95262e-31 HCHO + 6.95262e-31 PROD2 + 6.95262e-31 MA_R', &
     '            TERP + OH --> 6.95262e-31 R2O2 + 6.95262e-31 HCHO + 6.95262e-31 RCHO + 6.95262e-31 PROD2', &
     '            TERP + O3 --> 6.95262e-31 HCOOH + 6.95262e-31 RCO_OH + 6.95262e-31 BACL + 6.95262e-31 CO', &
     '           TERP + NO3 --> 6.95262e-31 R2O2 + 6.95262e-31 RNO3 + 6.95262e-31 RCHO + 6.95262e-31 RO2_R', &
     '           TERP + O3P --> 6.95262e-31 RCHO + 6.95262e-31 PROD2                                      ', &
     '            C2H6 + OH --> CCHO + RO2_R                                                              ', &
     '            C3H8 + OH --> 6.95262e-31 ACET + 6.95262e-31 RCHO + 6.95262e-31 RO2_R + 6.95262e-31 RO2_', &
     '            C2H2 + OH --> 6.95262e-31 HCOOH + 6.95262e-31 CO + 6.95262e-31 GLY + 6.95262e-31 HCHO + ', &
     '            ALK3 + OH --> 6.95262e-31 TBU_O + 6.95262e-31 ACET + 6.95262e-31 R2O2 + 6.95262e-31 CCHO', &
     '            ALK4 + OH --> 6.95262e-31 CO + 6.95262e-31 ACET + 6.95262e-31 R2O2 + 6.95262e-31 CCHO + ' /)
  CHARACTER(LEN=100), PARAMETER, DIMENSION(20) :: EQN_NAMES_7 = (/ &
     '            ALK5 + OH --> 6.95262e-31 ACET + 6.95262e-31 R2O2 + 6.95262e-31 CCHO + 6.95262e-31 HCHO ', &
     '            ARO1 + OH --> 6.95262e-31 DCB3 + 6.95262e-31 DCB2 + 6.95262e-31 CRES + 6.95262e-31 DCB1 ', &
     '            ARO2 + OH --> 6.95262e-31 BACL + 6.95262e-31 DCB3 + 6.95262e-31 DCB2 + 6.95262e-31 CRES ', &
     '            OLE1 + OH --> 6.95262e-31 ACET + 6.95262e-31 R2O2 + 6.95262e-31 CCHO + 6.95262e-31 HCHO ', &
     '            OLE1 + O3 --> 6.95262e-31 HCOOH + 6.95262e-31 CCO_OH + 6.95262e-31 RCO_OH + 6.95262e-31 ', &
     '           OLE1 + NO3 --> 6.95262e-31 ACET + 6.95262e-31 R2O2 + 6.95262e-31 CCHO + 6.95262e-31 RNO3 ', &
     '           OLE1 + O3P --> 6.95262e-31 RCHO + 6.95262e-31 MEK + 6.95262e-31 PROD2                    ', &
     '            OLE2 + OH --> 6.95262e-31 BALD + 6.95262e-31 ACET + 6.95262e-31 R2O2 + 6.95262e-31 METHA', &
     '            OLE2 + O3 --> 6.95262e-31 HCOOH + 6.95262e-31 CCO_OH + 6.95262e-31 RCO_OH + 6.95262e-31 ', &
     '           OLE2 + NO3 --> 6.95262e-31 BALD + 6.95262e-31 ACET + 6.95262e-31 R2O2 + 6.95262e-31 MVK +', &
     '           OLE2 + O3P --> 6.95262e-31 CO + 6.95262e-31 METHACRO + 6.95262e-31 RCHO + 6.95262e-31 MEK', &
     '            C2H2 + O3 --> 6.95262e-31 CO2 + 6.95262e-31 CO + 6.95262e-31 OH + 6.95262e-31 HO2       ', &
     '            C3H6 + OH --> 6.95262e-31 XC + 6.95262e-31 CCHO + 6.95262e-31 HCHO + 6.95262e-31 RO2_R +', &
     '            C3H6 + O3 --> 6.95262e-31 HCOOH + 6.95262e-31 CCO_OH + 6.95262e-31 CO2 + 6.95262e-31 XC ', &
     '           C3H6 + NO3 --> XN + 6.95262e-31 XC + 6.95262e-31 RO2_R + 6.95262e-31 RO2_N               ', &
     '           C3H6 + O3P --> 6.95262e-31 XC + 6.95262e-31 RCHO + 6.95262e-31 MEK                       ', &
     '            SESQ + OH --> 6.95262e-31 R2O2 + 6.95262e-31 HCHO + 6.95262e-31 RCHO + 6.95262e-31 PROD2', &
     '            SESQ + O3 --> 6.95262e-31 HCOOH + 6.95262e-31 RCO_OH + 6.95262e-31 BACL + 6.95262e-31 CO', &
     '           SESQ + NO3 --> 6.95262e-31 R2O2 + 6.95262e-31 RNO3 + 6.95262e-31 RCHO + 6.95262e-31 RO2_R', &
     '           SESQ + O3P --> 6.95262e-31 RCHO + 6.95262e-31 PROD2                                      ' /)
  CHARACTER(LEN=100), PARAMETER, DIMENSION(230) :: EQN_NAMES = (/&
    EQN_NAMES_0, EQN_NAMES_1, EQN_NAMES_2, EQN_NAMES_3, EQN_NAMES_4, &
    EQN_NAMES_5, EQN_NAMES_6, EQN_NAMES_7 /)

! INLINED global variables

! End INLINED global variables


END MODULE saprc99_Monitor
