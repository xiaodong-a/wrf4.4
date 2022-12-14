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
! File                 : mozart_mosaic_4bin_Monitor.f90
! Time                 : Mon Sep 26 23:37:13 2022
! Working directory    : /home/ess/xiaodong/WORK/WRFCHEM/model/WRF4.4/chem/KPP/mechanisms/mozart_mosaic_4bin
! Equation file        : mozart_mosaic_4bin.kpp
! Output root filename : mozart_mosaic_4bin
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



MODULE mozart_mosaic_4bin_Monitor


  CHARACTER(LEN=12), PARAMETER, DIMENSION(90) :: SPC_NAMES_0 = (/ &
     'HONO        ','SO4         ','NUME        ', &
     'DEN         ','BIOG1_c     ','BIOG1_o     ', &
     'SMPA        ','VOCA        ','SMPBB       ', &
     'VOCBB       ','NH3         ','C2H6        ', &
     'C3H8        ','BIGENE      ','N2O         ', &
     'SO2         ','PBZNIT      ','H2O2        ', &
     'C2H2        ','BENZENE     ','BEPOMUC     ', &
     'PHENOL      ','EO          ','TOLUENE     ', &
     'CRESOL      ','XOOH        ','PHENOOH     ', &
     'C6H5OOH     ','BENZOOH     ','BIGALD2     ', &
     'TEPOMUC     ','BZOOH       ','BZALD       ', &
     'XYLOLOOH    ','XYLENOOH    ','N2O5        ', &
     'XYLENES     ','XYLOL       ','DMS         ', &
     'BIGALD4     ','C2H5OH      ','BIGALD3     ', &
     'BIGALD1     ','H2          ','C2H5OOH     ', &
     'C3H7OOH     ','ROOH        ','ENEO2       ', &
     'MEKOOH      ','CH3OOH      ','MACROOH     ', &
     'HO2NO2      ','HYDRALD     ','BIGALK      ', &
     'C2H4        ','HOCH2OO     ','TOLOOH      ', &
     'PHENO2      ','PHENO       ','EO2         ', &
     'BZOO        ','MEK         ','CH3COOH     ', &
     'CH3COOOH    ','TERPOOH     ','BENZO2      ', &
     'ONIT        ','PAN         ','XYLOLO2     ', &
     'POOH        ','HNO3        ','CH4         ', &
     'HMPROPO2    ','ACBZO2      ','ISOPOOH     ', &
     'MBOOOH      ','MPAN        ','HCOOH       ', &
     'TERP2OOH    ','O1D_CB4     ','C6H5O2      ', &
     'APIN        ','BPIN        ','ALKOOH      ', &
     'MEKO2       ','PO2         ','MALO2       ', &
     'TOLO2       ','MBO         ','XYLENO2     ' /)
  CHARACTER(LEN=12), PARAMETER, DIMENSION(48) :: SPC_NAMES_1 = (/ &
     'DICARBO2    ','O           ','C3H7O2      ', &
     'MDIALO2     ','CH3OH       ','GLYOXAL     ', &
     'C2H5O2      ','BIGALD      ','ISOPNO3     ', &
     'BCARY       ','LIMON       ','MBONO3O2    ', &
     'HMPROP      ','MYRC        ','CH3COCH3    ', &
     'ISOP        ','CO          ','GLYALD      ', &
     'C3H6        ','TERPROD1    ','MBOO2       ', &
     'CH3CHO      ','ALKO2       ','TERPROD2    ', &
     'MACR        ','ONITR       ','CH3COCHO    ', &
     'TERPO2      ','NTERPO2     ','TERP2O2     ', &
     'RO2         ','HYAC        ','XO2         ', &
     'MVK         ','MACRO2      ','CH2O        ', &
     'ISOPO2      ','CH3CO3      ','CH3O2       ', &
     'MCO3        ','HO2         ','O3          ', &
     'NO          ','NO2         ','NO3         ', &
     'OH          ','H2O         ','M           ' /)
  CHARACTER(LEN=12), PARAMETER, DIMENSION(138) :: SPC_NAMES = (/&
    SPC_NAMES_0, SPC_NAMES_1 /)

  INTEGER, DIMENSION(1) :: LOOKAT
  INTEGER, DIMENSION(1) :: MONITOR
  CHARACTER(LEN=12), DIMENSION(1) :: SMASS
  CHARACTER(LEN=100), PARAMETER, DIMENSION(30) :: EQN_NAMES_0 = (/ &
     '                    M --> 6.95272e-31 O                                                             ', &
     '                   O3 --> O1D_CB4                                                                   ', &
     '                   O3 --> O                                                                         ', &
     '                  N2O --> O1D_CB4                                                                   ', &
     '                  NO2 --> O + NO                                                                    ', &
     '                 N2O5 --> NO2 + NO3                                                                 ', &
     '                 HNO3 --> NO2 + OH                                                                  ', &
     '                  NO3 --> 6.95272e-31 O3 + 6.95272e-31 NO + 6.95272e-31 NO2                         ', &
     '               HO2NO2 --> 6.95272e-31 HO2 + 6.95272e-31 NO2 + 6.95272e-31 NO3 + 6.95272e-31 OH      ', &
     '               CH3OOH --> CH2O + HO2 + OH                                                           ', &
     '                 CH2O --> CO + 6.95272e-31 HO2                                                      ', &
     '                 CH2O --> H2 + CO                                                                   ', &
     '                 H2O2 --> 6.95272e-31 OH                                                            ', &
     '               CH3CHO --> CO + CH3O2 + HO2                                                          ', &
     '                 POOH --> CH3CHO + CH2O + HO2 + OH                                                  ', &
     '             CH3COOOH --> CH3O2 + OH                                                                ', &
     '                  PAN --> 6.95272e-31 CH3CO3 + 6.95272e-31 CH3O2 + 6.95272e-31 NO2 + 6.95272e-31 NO3', &
     '                 MPAN --> MCO3 + NO2                                                                ', &
     '                 MACR --> 6.95272e-31 CO + 6.95272e-31 CH2O + 6.95272e-31 CH3CO3 + 6.95272e-31 MCO3 ', &
     '                  MVK --> 6.95272e-31 CO + 6.95272e-31 C3H6 + 6.95272e-31 CH3CO3 + 6.95272e-31 CH3O2', &
     '              C2H5OOH --> CH3CHO + HO2 + OH                                                         ', &
     '              C3H7OOH --> 6.95272e-31 CH3COCH3 + HO2 + OH                                           ', &
     '                 ROOH --> CH2O + CH3CO3 + OH                                                        ', &
     '             CH3COCH3 --> CH3CO3 + CH3O2                                                            ', &
     '             CH3COCHO --> CO + CH3CO3 + HO2                                                         ', &
     '                 XOOH --> OH                                                                        ', &
     '                ONITR --> CO + CH2O + HO2 + NO2                                                     ', &
     '              ISOPOOH --> 6.95272e-31 MACR + 6.95272e-31 MVK + 6.95272e-31 CH2O + HO2               ', &
     '                 HYAC --> CH2O + CH3CO3 + HO2                                                       ', &
     '               GLYALD --> CO + CH2O + 6.95272e-31 HO2                                               ' /)
  CHARACTER(LEN=100), PARAMETER, DIMENSION(30) :: EQN_NAMES_1 = (/ &
     '                  MEK --> C2H5O2 + CH3CO3                                                           ', &
     '               BIGALD --> 6.95272e-31 GLYOXAL + 6.95272e-31 CO + 6.95272e-31 CH3COCHO + 6.95272e-31 ', &
     '              GLYOXAL --> 6.95272e-31 CO + 6.95272e-31 HO2                                          ', &
     '               ALKOOH --> 6.95272e-31 MEK + 6.95272e-31 CH3COCH3 + 6.95272e-31 CH3CHO + 6.95272e-31 ', &
     '               MEKOOH --> CH3CHO + CH3CO3 + OH                                                      ', &
     '               TOLOOH --> 6.95272e-31 GLYOXAL + 6.95272e-31 BIGALD + 6.95272e-31 CH3COCHO + OH      ', &
     '              BIGALD1 --> 6.95272e-31 MALO2 + HO2                                                   ', &
     '              BEPOMUC --> BIGALD1 + 6.95272e-31 CO + 6.95272e-31 HO2                                ', &
     '              TEPOMUC --> 6.95272e-31 CO + 6.95272e-31 CH3CO3 + HO2                                 ', &
     '              BIGALD2 --> 6.95272e-31 DICARBO2 + 6.95272e-31 HO2                                    ', &
     '              BIGALD3 --> 6.95272e-31 MDIALO2 + 6.95272e-31 CO + 6.95272e-31 HO2                    ', &
     '              BIGALD4 --> CO + CH3COCHO + CH3CO3 + HO2                                              ', &
     '               MBOOOH --> 6.95272e-31 HMPROP + 6.95272e-31 CH3COCH3 + 6.95272e-31 GLYALD + 6.95272e-', &
     '               HMPROP --> CH3COCH3 + CO + 6.95272e-31 HO2                                           ', &
     '             TERPROD1 --> CO + TERPROD2 + HO2                                                       ', &
     '             TERPROD2 --> 6.95272e-31 CH3COCH3 + 6.95272e-31 CO + 6.95272e-31 RO2 + 6.95272e-31 CH2O', &
     '              TERPOOH --> 6.95272e-31 CH3COCH3 + TERPROD1 + 6.95272e-31 CH2O + HO2 + OH             ', &
     '             TERP2OOH --> 6.95272e-31 CH3COCH3 + 6.95272e-31 CO + 6.95272e-31 GLYALD + TERPROD2 + 6.', &
     '                 HONO --> NO + OH                                                                   ', &
     '                O + M --> O3                                                                        ', &
     '               O + O3 --> M                                                                         ', &
     '          O1D_CB4 + M --> O                                                                         ', &
     '        O1D_CB4 + H2O --> 6.95272e-31 OH                                                            ', &
     '         H2 + O1D_CB4 --> HO2 + OH                                                                  ', &
     '              H2 + OH --> HO2 + H2O                                                                 ', &
     '               O + OH --> HO2                                                                       ', &
     '              O + HO2 --> OH                                                                        ', &
     '              O3 + OH --> HO2                                                                       ', &
     '             HO2 + O3 --> OH                                                                        ', &
     '6.95272e-31 HO2 + H2O --> H2O2                                                                      ' /)
  CHARACTER(LEN=100), PARAMETER, DIMENSION(30) :: EQN_NAMES_2 = (/ &
     '            H2O2 + OH --> HO2 + H2O                                                                 ', &
     '             HO2 + OH --> H2O                                                                       ', &
     '       6.95272e-31 OH --> O + H2O                                                                   ', &
     '       6.95272e-31 OH --> H2O2                                                                      ', &
     '        N2O + O1D_CB4 --> 6.95272e-31 NO                                                            ', &
     '        N2O + O1D_CB4 --> M                                                                         ', &
     '             HO2 + NO --> NO2 + OH                                                                  ', &
     '              O3 + NO --> NO2                                                                       ', &
     '              O + NO2 --> NO                                                                        ', &
     '             O3 + NO2 --> NO3                                                                       ', &
     '            HO2 + NO3 --> NO2 + OH                                                                  ', &
     '            NO2 + NO3 --> N2O5                                                                      ', &
     '                 N2O5 --> NO2 + NO3                                                                 ', &
     '             NO2 + OH --> HNO3                                                                      ', &
     '            HNO3 + OH --> NO3 + H2O                                                                 ', &
     '             NO + NO3 --> 6.95272e-31 NO2                                                           ', &
     '            HO2 + NO2 --> HO2NO2                                                                    ', &
     '          HO2NO2 + OH --> NO2 + H2O                                                                 ', &
     '               HO2NO2 --> HO2 + NO2                                                                 ', &
     '             N2O5 + M --> 6.95272e-31 HNO3                                                          ', &
     '                  NO3 --> HNO3                                                                      ', &
     '                  NO2 --> 6.95272e-31 HNO3 + 6.95272e-31 NO + 6.95272e-31 OH                        ', &
     '             CH4 + OH --> CH3O2 + H2O                                                               ', &
     '        CH4 + O1D_CB4 --> 6.95272e-31 H2 + 6.95272e-31 CH2O + 6.95272e-31 CH3O2 + 6.95272e-31 HO2 + ', &
     '           CH3O2 + NO --> NUME + CH2O + HO2 + NO2                                                   ', &
     '    6.95272e-31 CH3O2 --> DEN + 6.95272e-31 CH2O + 6.95272e-31 HO2                                  ', &
     '    6.95272e-31 CH3O2 --> DEN + CH3OH + CH2O                                                        ', &
     '          CH3O2 + HO2 --> DEN + CH3OOH                                                              ', &
     '          CH3OOH + OH --> 6.95272e-31 CH2O + 6.95272e-31 CH3O2 + 6.95272e-31 OH + H2O               ', &
     '           CH2O + NO3 --> HNO3 + CO + HO2                                                           ' /)
  CHARACTER(LEN=100), PARAMETER, DIMENSION(30) :: EQN_NAMES_3 = (/ &
     '            CH2O + OH --> CO + HO2 + H2O                                                            ', &
     '              CO + OH --> HO2                                                                       ', &
     '            C2H4 + OH --> 6.95272e-31 EO2 + 6.95272e-31 CH2O + 6.95272e-31 HO2                      ', &
     '            C2H4 + O3 --> 6.95272e-31 HCOOH + 6.95272e-31 CO + CH2O + 6.95272e-31 HO2 + 6.95272e-31 ', &
     '             SO2 + OH --> SO4                                                                       ', &
     '         GLYOXAL + OH --> CO + HO2                                                                  ', &
     '             EO2 + NO --> NUME + EO + NO2                                                           ', &
     '               EO + M --> GLYALD + HO2                                                              ', &
     '                   EO --> 6.95272e-31 CH2O + HO2                                                    ', &
     '            C2H6 + OH --> C2H5O2                                                                    ', &
     '          C2H5O2 + NO --> NUME + CH3CHO + HO2 + NO2                                                 ', &
     '         C2H5O2 + HO2 --> DEN + C2H5OOH                                                             ', &
     '       C2H5O2 + CH3O2 --> DEN + 6.95272e-31 C2H5OH + 6.95272e-31 CH3OH + 6.95272e-31 CH3CHO + 6.9527', &
     '         C2H5OOH + OH --> 6.95272e-31 C2H5O2 + 6.95272e-31 CH3CHO + 6.95272e-31 OH                  ', &
     '            C3H6 + OH --> PO2                                                                       ', &
     '            C3H6 + O3 --> 6.95272e-31 CH3COOH + 6.95272e-31 CH4 + 6.95272e-31 CO + 6.95272e-31 CH3CH', &
     '           C3H6 + NO3 --> ONIT                                                                      ', &
     '             PO2 + NO --> NUME + CH3CHO + CH2O + HO2 + NO2                                          ', &
     '            PO2 + HO2 --> DEN + POOH                                                                ', &
     '            POOH + OH --> 6.95272e-31 PO2 + 6.95272e-31 HYAC + 6.95272e-31 OH                       ', &
     '          CH3CHO + OH --> CH3CO3                                                                    ', &
     '         CH3CHO + NO3 --> HNO3 + CH3CO3                                                             ', &
     '          CH3CO3 + NO --> NUME + CH3O2 + NO2                                                        ', &
     '         CH3CO3 + NO2 --> PAN                                                                       ', &
     '         CH3CO3 + HO2 --> DEN + 6.95272e-31 CH3COOH + 6.95272e-31 CH3COOOH + 6.95272e-31 O3         ', &
     '       CH3CO3 + CH3O2 --> DEN + 6.95272e-31 CH3COOH + CH2O + 6.95272e-31 CH3O2 + 6.95272e-31 HO2    ', &
     '        CH3COOOH + OH --> 6.95272e-31 CH2O + 6.95272e-31 CH3CO3                                     ', &
     '                  PAN --> CH3CO3 + NO2                                                              ', &
     '   6.95272e-31 CH3CO3 --> DEN + 6.95272e-31 CH3O2                                                   ', &
     '            C3H8 + OH --> C3H7O2                                                                    ' /)
  CHARACTER(LEN=100), PARAMETER, DIMENSION(30) :: EQN_NAMES_4 = (/ &
     '          C3H7O2 + NO --> NUME + 6.95272e-31 CH3COCH3 + 6.95272e-31 CH3CHO + HO2 + NO2              ', &
     '         C3H7O2 + HO2 --> DEN + C3H7OOH                                                             ', &
     '       C3H7O2 + CH3O2 --> DEN + 6.95272e-31 CH3COCH3 + CH2O + HO2                                   ', &
     '         C3H7OOH + OH --> C3H7O2                                                                    ', &
     '        CH3COCH3 + OH --> RO2                                                                       ', &
     '             RO2 + NO --> NUME + CH2O + CH3CO3 + NO2                                                ', &
     '            RO2 + HO2 --> DEN + ROOH                                                                ', &
     '          RO2 + CH3O2 --> DEN + 6.95272e-31 CH3OH + 6.95272e-31 CH3COCHO + 6.95272e-31 HYAC + 6.9527', &
     '            ROOH + OH --> RO2                                                                       ', &
     '          BIGENE + OH --> ENEO2                                                                     ', &
     '           ENEO2 + NO --> NUME + 6.95272e-31 CH3COCH3 + CH3CHO + 6.95272e-31 CH2O + HO2 + NO2       ', &
     '          BIGALK + OH --> ALKO2                                                                     ', &
     '           ALKO2 + NO --> NUME + 6.95272e-31 MEK + 6.95272e-31 ONIT + 6.95272e-31 CH3COCH3 + 6.95272', &
     '          ALKO2 + HO2 --> DEN + ALKOOH                                                              ', &
     '          ALKOOH + OH --> ALKO2                                                                     ', &
     '            ONIT + OH --> CH3COCHO + NO2                                                            ', &
     '             MEK + OH --> MEKO2                                                                     ', &
     '           MEKO2 + NO --> NUME + CH3CHO + CH3CO3 + NO2                                              ', &
     '          MEKO2 + HO2 --> DEN + MEKOOH                                                              ', &
     '          MEKOOH + OH --> MEKO2                                                                     ', &
     '          TOLO2 + HO2 --> DEN + TOLOOH                                                              ', &
     '          TOLOOH + OH --> TOLO2                                                                     ', &
     '            ISOP + OH --> ISOPO2                                                                    ', &
     '            ISOP + O3 --> 6.95272e-31 CH3COOH + 6.95272e-31 CO + 6.95272e-31 C3H6 + 6.95272e-31 MACR', &
     '          ISOPO2 + NO --> NUME + 6.95272e-31 HYDRALD + 6.95272e-31 GLYOXAL + 6.95272e-31 GLYALD + 6.', &
     '         ISOPO2 + NO3 --> 6.95272e-31 HYDRALD + 6.95272e-31 GLYOXAL + 6.95272e-31 GLYALD + 6.95272e-', &
     '         ISOPO2 + HO2 --> DEN + ISOPOOH                                                             ', &
     '         ISOPOOH + OH --> 6.95272e-31 XO2 + 6.95272e-31 ISOPO2                                      ', &
     '       ISOPO2 + CH3O2 --> DEN + 6.95272e-31 HYDRALD + 6.95272e-31 CH3OH + 6.95272e-31 MACR + 6.95272', &
     '      ISOPO2 + CH3CO3 --> DEN + 6.95272e-31 HYDRALD + 6.95272e-31 MACR + 6.95272e-31 MVK + 6.95272e-' /)
  CHARACTER(LEN=100), PARAMETER, DIMENSION(30) :: EQN_NAMES_5 = (/ &
     '             MVK + OH --> MACRO2                                                                    ', &
     '             MVK + O3 --> 6.95272e-31 CO + 6.95272e-31 CH3CHO + 6.95272e-31 CH3COCHO + 6.95272e-31 C', &
     '            MACR + OH --> 6.95272e-31 MACRO2 + 6.95272e-31 MCO3                                     ', &
     '            MACR + O3 --> 6.95272e-31 CO + 6.95272e-31 CH3COCHO + 6.95272e-31 CH2O + 6.95272e-31 HO2', &
     '          MACRO2 + NO --> NUME + 6.95272e-31 CO + 6.95272e-31 GLYALD + 6.95272e-31 CH3COCHO + 6.9527', &
     '          MACRO2 + NO --> NUME + 6.95272e-31 ONITR                                                  ', &
     '         MACRO2 + NO3 --> 6.95272e-31 CO + 6.95272e-31 GLYALD + 6.95272e-31 CH3COCHO + 6.95272e-31 H', &
     '         MACRO2 + HO2 --> DEN + MACROOH                                                             ', &
     '       MACRO2 + CH3O2 --> DEN + 6.95272e-31 CH3OH + 6.95272e-31 CO + 6.95272e-31 GLYALD + 6.95272e-3', &
     '      MACRO2 + CH3CO3 --> DEN + 6.95272e-31 CO + 6.95272e-31 GLYALD + 6.95272e-31 CH3COCHO + 6.95272', &
     '         MACROOH + OH --> 6.95272e-31 MACRO2 + 6.95272e-31 MCO3 + 6.95272e-31 HO2 + 6.95272e-31 OH  ', &
     '            MCO3 + NO --> NUME + CH2O + CH3CO3 + NO2                                                ', &
     '           MCO3 + NO3 --> CH2O + CH3CO3 + NO2                                                       ', &
     '           MCO3 + HO2 --> DEN + 6.95272e-31 CH3COOH + 6.95272e-31 CH3COOOH + 6.95272e-31 O3         ', &
     '         CH3O2 + MCO3 --> DEN + 6.95272e-31 CH2O + CH3CO3 + HO2                                     ', &
     '        CH3CO3 + MCO3 --> DEN + CH2O + CH3CO3 + CH3O2                                               ', &
     '     6.95272e-31 MCO3 --> DEN + 6.95272e-31 CH2O + 6.95272e-31 CH3CO3                               ', &
     '       MCO3 + NO2 + M --> MPAN                                                                      ', &
     '             MPAN + M --> MCO3 + NO2                                                                ', &
     '         BENZENE + OH --> 6.95272e-31 BEPOMUC + 6.95272e-31 PHENOL + 6.95272e-31 BENZO2 + 6.95272e-3', &
     '          PHENOL + OH --> 6.95272e-31 PHENO2 + 6.95272e-31 PHENO + 6.95272e-31 HO2                  ', &
     '          PHENO2 + NO --> NUME + 6.95272e-31 GLYOXAL + HO2 + NO2                                    ', &
     '         PHENO2 + HO2 --> DEN + PHENOOH                                                             ', &
     '         PHENOOH + OH --> PHENO2                                                                    ', &
     '          PHENO + NO2 --> M                                                                         ', &
     '           PHENO + O3 --> C6H5O2                                                                    ', &
     '          C6H5O2 + NO --> NUME + PHENO + NO2                                                        ', &
     '         C6H5O2 + HO2 --> DEN + C6H5OOH                                                             ', &
     '         C6H5OOH + OH --> C6H5O2                                                                    ', &
     '          BENZO2 + NO --> NUME + 6.95272e-31 BIGALD1 + GLYOXAL + HO2 + NO2                          ' /)
  CHARACTER(LEN=100), PARAMETER, DIMENSION(30) :: EQN_NAMES_6 = (/ &
     '         BENZO2 + HO2 --> DEN + BENZOOH                                                             ', &
     '         BENZOOH + OH --> BENZO2                                                                    ', &
     '          MALO2 + NO2 --> M                                                                         ', &
     '           MALO2 + NO --> NUME + 6.95272e-31 GLYOXAL + 6.95272e-31 CO + 6.95272e-31 HO2             ', &
     '          MALO2 + HO2 --> DEN + 6.95272e-31 GLYOXAL + 6.95272e-31 CO + 6.95272e-31 HO2              ', &
     '         TOLUENE + OH --> 6.95272e-31 CRESOL + 6.95272e-31 TEPOMUC + 6.95272e-31 BZOO + 6.95272e-31 ', &
     '          CRESOL + OH --> 6.95272e-31 PHENO2 + 6.95272e-31 PHENO + 6.95272e-31 HO2                  ', &
     '           BZOO + HO2 --> DEN + BZOOH                                                               ', &
     '           BZOOH + OH --> BZOO                                                                      ', &
     '            BZOO + NO --> NUME + BZALD + HO2 + NO2                                                  ', &
     '           BZALD + OH --> ACBZO2                                                                    ', &
     '         ACBZO2 + NO2 --> PBZNIT                                                                    ', &
     '               PBZNIT --> ACBZO2 + NO2                                                              ', &
     '          ACBZO2 + NO --> NUME + C6H5O2 + NO2                                                       ', &
     '         ACBZO2 + HO2 --> DEN + 6.95272e-31 C6H5O2 + 6.95272e-31 OH                                 ', &
     '           TOLO2 + NO --> NUME + 6.95272e-31 BIGALD2 + 6.95272e-31 BIGALD3 + 6.95272e-31 BIGALD1 + 6', &
     '       DICARBO2 + HO2 --> DEN + 6.95272e-31 CO + 6.95272e-31 CH3COCHO + 6.95272e-31 CH3O2 + 6.95272e', &
     '        DICARBO2 + NO --> NUME + 6.95272e-31 CO + 6.95272e-31 CH3COCHO + 6.95272e-31 CH3O2 + 6.95272', &
     '        MDIALO2 + HO2 --> DEN + 6.95272e-31 GLYOXAL + 6.95272e-31 CO + 6.95272e-31 CH3COCHO + 6.9527', &
     '         MDIALO2 + NO --> NUME + 6.95272e-31 GLYOXAL + 6.95272e-31 CO + 6.95272e-31 CH3COCHO + 6.952', &
     '       DICARBO2 + NO2 --> M                                                                         ', &
     '        MDIALO2 + NO2 --> M                                                                         ', &
     '         XYLENES + OH --> 6.95272e-31 TEPOMUC + 6.95272e-31 XYLOL + 6.95272e-31 BZOO + 6.95272e-31 X', &
     '           XYLOL + OH --> 6.95272e-31 PHENO + 6.95272e-31 XYLOLO2 + 6.95272e-31 HO2                 ', &
     '         XYLOLO2 + NO --> NUME + 6.95272e-31 GLYOXAL + 6.95272e-31 CH3COCHO + HO2 + NO2             ', &
     '        XYLOLO2 + HO2 --> DEN + XYLOLOOH                                                            ', &
     '        XYLOLOOH + OH --> XYLOLO2                                                                   ', &
     '        XYLENO2 + HO2 --> DEN + XYLENOOH                                                            ', &
     '        XYLENOOH + OH --> XYLENO2                                                                   ', &
     '         XYLENO2 + NO --> NUME + 6.95272e-31 BIGALD2 + 6.95272e-31 BIGALD4 + 6.95272e-31 BIGALD3 + 6' /)
  CHARACTER(LEN=100), PARAMETER, DIMENSION(30) :: EQN_NAMES_7 = (/ &
     '            APIN + OH --> TERPO2                                                                    ', &
     '            BPIN + OH --> TERPO2                                                                    ', &
     '           LIMON + OH --> TERPO2                                                                    ', &
     '            MYRC + OH --> TERPO2                                                                    ', &
     '           BCARY + OH --> TERPO2                                                                    ', &
     '            APIN + O3 --> 6.95272e-31 BIGALK + 6.95272e-31 HCOOH + 6.95272e-31 BIGALD + 6.95272e-31 ', &
     '            BPIN + O3 --> 6.95272e-31 BIGALK + 6.95272e-31 HCOOH + 6.95272e-31 BIGALD + 6.95272e-31 ', &
     '           LIMON + O3 --> 6.95272e-31 BIGALK + 6.95272e-31 HCOOH + 6.95272e-31 BIGALD + 6.95272e-31 ', &
     '            MYRC + O3 --> 6.95272e-31 BIGALK + 6.95272e-31 HCOOH + 6.95272e-31 BIGALD + 6.95272e-31 ', &
     '           BCARY + O3 --> 6.95272e-31 BIGALK + 6.95272e-31 HCOOH + 6.95272e-31 BIGALD + 6.95272e-31 ', &
     '          LIMON + NO3 --> NTERPO2                                                                   ', &
     '           MYRC + NO3 --> NTERPO2                                                                   ', &
     '          BCARY + NO3 --> NTERPO2                                                                   ', &
     '          TERPO2 + NO --> NUME + 6.95272e-31 CH3COCH3 + 6.95272e-31 TERPROD1 + 6.95272e-31 ONITR + 6', &
     '         TERPO2 + HO2 --> DEN + TERPOOH                                                             ', &
     '       TERPO2 + CH3O2 --> DEN + 6.95272e-31 CH3OH + 6.95272e-31 CH3COCH3 + TERPROD1 + 6.95272e-31 CH', &
     '         TERPOOH + OH --> TERPO2                                                                    ', &
     '        TERP2OOH + OH --> TERP2O2                                                                   ', &
     '        TERPROD1 + OH --> TERP2O2                                                                   ', &
     '       TERPROD1 + NO3 --> 6.95272e-31 NTERPO2 + 6.95272e-31 TERP2O2                                 ', &
     '         TERP2O2 + NO --> NUME + 6.95272e-31 CH3COCH3 + 6.95272e-31 CO + 6.95272e-31 GLYALD + 6.9527', &
     '        TERP2O2 + HO2 --> DEN + TERP2OOH                                                            ', &
     '      TERP2O2 + CH3O2 --> DEN + 6.95272e-31 CH3OH + 6.95272e-31 CH3COCH3 + 6.95272e-31 CO + 6.95272e', &
     '        TERPROD2 + OH --> 6.95272e-31 CH3COCH3 + 6.95272e-31 CO + 6.95272e-31 RO2 + 6.95272e-31 CH2O', &
     '         NTERPO2 + NO --> NUME + 6.95272e-31 TERPROD1 + 6.95272e-31 ONITR + 6.95272e-31 NO2         ', &
     '        NTERPO2 + HO2 --> DEN + ONITR                                                               ', &
     '      NTERPO2 + CH3O2 --> DEN + 6.95272e-31 CH3OH + 6.95272e-31 TERPROD1 + 6.95272e-31 ONITR + 6.952', &
     '        NTERPO2 + NO3 --> TERPROD1 + 6.95272e-31 NO2                                                ', &
     '         CH3COOH + OH --> CH3O2                                                                     ', &
     '           ISOP + NO3 --> ISOPNO3                                                                   ' /)
  CHARACTER(LEN=100), PARAMETER, DIMENSION(30) :: EQN_NAMES_8 = (/ &
     '         ISOPNO3 + NO --> NUME + 6.95272e-31 MACR + 6.95272e-31 ONITR + 6.95272e-31 MVK + 6.95272e-3', &
     '        ISOPNO3 + NO3 --> 6.95272e-31 MACR + 6.95272e-31 ONITR + 6.95272e-31 MVK + 6.95272e-31 CH2O ', &
     '        ISOPNO3 + HO2 --> DEN + 6.95272e-31 MACR + 6.95272e-31 ONITR + 6.95272e-31 MVK + 6.95272e-31', &
     '        CH3COCHO + OH --> CO + CH3CO3                                                               ', &
     '       CH3COCHO + NO3 --> HNO3 + CO + CH3CO3                                                        ', &
     '           ONITR + OH --> HYDRALD + HO2 + 6.95272e-31 NO2                                           ', &
     '          ONITR + NO3 --> HYDRALD + HO2 + NO2                                                       ', &
     '         HYDRALD + OH --> XO2                                                                       ', &
     '             XO2 + NO --> NUME + 6.95272e-31 GLYOXAL + 6.95272e-31 CO + 6.95272e-31 GLYALD + 6.95272', &
     '            XO2 + NO3 --> 6.95272e-31 GLYOXAL + 6.95272e-31 CO + 6.95272e-31 GLYALD + 6.95272e-31 CH', &
     '            XO2 + HO2 --> DEN + XOOH                                                                ', &
     '          XO2 + CH3O2 --> DEN + 6.95272e-31 CH3OH + 6.95272e-31 GLYOXAL + 6.95272e-31 CO + 6.95272e-', &
     '         XO2 + CH3CO3 --> DEN + 6.95272e-31 GLYOXAL + 6.95272e-31 CO + 6.95272e-31 GLYALD + 6.95272e', &
     '            XOOH + OH --> XO2                                                                       ', &
     '            XOOH + OH --> OH                                                                        ', &
     '           CH3OH + OH --> CH2O + HO2                                                                ', &
     '          C2H5OH + OH --> CH3CHO + HO2                                                              ', &
     '            MPAN + OH --> 6.95272e-31 HYAC + 6.95272e-31 CH2O + 6.95272e-31 HO2 + 6.95272e-31 NO3   ', &
     '             PAN + OH --> CH2O + NO3                                                                ', &
     '            HYAC + OH --> CH3COCHO + HO2                                                            ', &
     '          GLYALD + OH --> 6.95272e-31 GLYOXAL + 6.95272e-31 CH2O + HO2                              ', &
     '             DMS + OH --> SO2                                                                       ', &
     '             DMS + OH --> 6.95272e-31 SO2 + 6.95272e-31 HO2                                         ', &
     '            DMS + NO3 --> SO2 + HNO3                                                                ', &
     '             NH3 + OH --> M                                                                         ', &
     '                  HO2 --> 6.95272e-31 H2O2                                                          ', &
     '   6.95272e-31 C2H5O2 --> DEN + 6.95272e-31 C2H5OH + 6.95272e-31 CH3CHO + 6.95272e-31 HO2           ', &
     '             MBO + OH --> MBOO2                                                                     ', &
     '           MBOO2 + NO --> NUME + 6.95272e-31 HMPROP + 6.95272e-31 CH3COCH3 + 6.95272e-31 GLYALD + 6.', &
     '        MBOO2 + CH3O2 --> DEN + 6.95272e-31 CH3OH + 6.95272e-31 HMPROP + 6.95272e-31 CH3COCH3 + 6.95' /)
  CHARACTER(LEN=100), PARAMETER, DIMENSION(22) :: EQN_NAMES_9 = (/ &
     '          HMPROP + OH --> HMPROPO2                                                                  ', &
     '        HMPROPO2 + NO --> NUME + CH3COCH3 + HO2 + NO2                                               ', &
     '       HMPROPO2 + HO2 --> DEN + 6.95272e-31 CH3COCH3 + 6.95272e-31 HO2 + 6.95272e-31 OH             ', &
     '          MBOO2 + HO2 --> DEN + MBOOOH                                                              ', &
     '          MBOOOH + OH --> 6.95272e-31 MBOO2 + 6.95272e-31 OH                                        ', &
     '             MBO + O3 --> 6.95272e-31 HCOOH + 6.95272e-31 HMPROP + 6.95272e-31 CH3COCH3 + 6.95272e-3', &
     '            MBO + NO3 --> MBONO3O2                                                                  ', &
     '       MBONO3O2 + HO2 --> DEN                                                                       ', &
     '        MBONO3O2 + NO --> NUME + 6.95272e-31 ONIT + 6.95272e-31 HMPROP + 6.95272e-31 CH3COCH3 + 6.95', &
     '       MBONO3O2 + NO3 --> 6.95272e-31 ONIT + 6.95272e-31 HMPROP + 6.95272e-31 CH3COCH3 + 6.95272e-31', &
     '            C2H2 + OH --> 6.95272e-31 HCOOH + 6.95272e-31 GLYOXAL + 6.95272e-31 CO + 6.95272e-31 HO2', &
     '           HCOOH + OH --> HO2 + H2O                                                                 ', &
     '           CH2O + HO2 --> HOCH2OO                                                                   ', &
     '              HOCH2OO --> CH2O + HO2                                                                ', &
     '         HOCH2OO + NO --> NUME + HCOOH + HO2 + NO2                                                  ', &
     '        HOCH2OO + HO2 --> DEN + HCOOH                                                               ', &
     '            VOCA + OH --> SMPA + OH                                                                 ', &
     '           VOCBB + OH --> SMPBB + OH                                                                ', &
     '            ISOP + OH --> BIOG1_c + ISOP + OH                                                       ', &
     '            APIN + OH --> BIOG1_o + APIN + OH                                                       ', &
     '            BPIN + OH --> BIOG1_o + BPIN + OH                                                       ', &
     '           LIMON + OH --> BIOG1_o + LIMON + OH                                                      ' /)
  CHARACTER(LEN=100), PARAMETER, DIMENSION(292) :: EQN_NAMES = (/&
    EQN_NAMES_0, EQN_NAMES_1, EQN_NAMES_2, EQN_NAMES_3, EQN_NAMES_4, &
    EQN_NAMES_5, EQN_NAMES_6, EQN_NAMES_7, EQN_NAMES_8, EQN_NAMES_9 /)

! INLINED global variables

! End INLINED global variables


END MODULE mozart_mosaic_4bin_Monitor
