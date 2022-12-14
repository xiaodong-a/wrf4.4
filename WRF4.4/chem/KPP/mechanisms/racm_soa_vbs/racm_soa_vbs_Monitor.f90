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
! File                 : racm_soa_vbs_Monitor.f90
! Time                 : Mon Sep 26 23:37:38 2022
! Working directory    : /home/ess/xiaodong/WORK/WRFCHEM/model/WRF4.4/chem/KPP/mechanisms/racm_soa_vbs
! Equation file        : racm_soa_vbs.kpp
! Output root filename : racm_soa_vbs
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



MODULE racm_soa_vbs_Monitor


  CHARACTER(LEN=12), PARAMETER, DIMENSION(90) :: SPC_NAMES_0 = (/ &
     'SULF        ','CO2         ','ORA1        ', &
     'ORA2        ','CVASOA1     ','CVASOA2     ', &
     'CVASOA3     ','CVASOA4     ','CVBSOA1     ', &
     'CVBSOA2     ','CVBSOA3     ','CVBSOA4     ', &
     'O1D         ','SO2         ','ISHP        ', &
     'TOL         ','XYL         ','HC5         ', &
     'N2O5        ','HC8         ','MAHP        ', &
     'HC3         ','UDD         ','NALD        ', &
     'ETH         ','CH4         ','OP1         ', &
     'MPAN        ','HNO4        ','HACE        ', &
     'HONO        ','HKET        ','O3P         ', &
     'PHO         ','H2O2        ','ADDT        ', &
     'ADDX        ','ETE         ','ADDC        ', &
     'PAA         ','ISON        ','SESQ        ', &
     'HNO3        ','PAN         ','API         ', &
     'CO          ','LIM         ','ISO         ', &
     'MBO         ','CSL         ','DIEN        ', &
     'MACP        ','GLY         ','TPAN        ', &
     'ETEP        ','OLTP        ','MACR        ', &
     'APIP        ','KET         ','ISOP        ', &
     'CSLP        ','MGLY        ','LIMP        ', &
     'HC5P        ','HC8P        ','HCHO        ', &
     'TOLP        ','XYLP        ','OLIP        ', &
     'ONIT        ','DCB         ','XO2         ', &
     'OLI         ','ALD         ','OLT         ', &
     'OLND        ','OLNN        ','TCO3        ', &
     'O3          ','HC3P        ','KETP        ', &
     'ACO3        ','MO2         ','HO          ', &
     'HO2         ','OP2         ','NO          ', &
     'NO2         ','NO3         ','ETHP        ' /)
  CHARACTER(LEN=12), PARAMETER, DIMENSION(2) :: SPC_NAMES_1 = (/ &
     'H2O         ','M           ' /)
  CHARACTER(LEN=12), PARAMETER, DIMENSION(92) :: SPC_NAMES = (/&
    SPC_NAMES_0, SPC_NAMES_1 /)

  INTEGER, DIMENSION(1) :: LOOKAT
  INTEGER, DIMENSION(1) :: MONITOR
  CHARACTER(LEN=12), DIMENSION(1) :: SMASS
  CHARACTER(LEN=100), PARAMETER, DIMENSION(30) :: EQN_NAMES_0 = (/ &
     '                  NO2 --> O3P + NO                                                                  ', &
     '                   O3 --> O1D                                                                       ', &
     '                   O3 --> O3P                                                                       ', &
     '                 HONO --> HO + NO                                                                   ', &
     '                 HNO3 --> HO + NO2                                                                  ', &
     '                 HNO4 --> 6.95307e-31 HO + 6.95307e-31 HO2 + 6.95307e-31 NO2 + 6.95307e-31 NO3      ', &
     '                  NO3 --> NO                                                                        ', &
     '                  NO3 --> O3P + NO2                                                                 ', &
     '                 H2O2 --> 6.95307e-31 HO                                                            ', &
     '                 HCHO --> CO                                                                        ', &
     '                 HCHO --> CO + 6.95307e-31 HO2                                                      ', &
     '                  ALD --> CO + MO2 + HO2                                                            ', &
     '                  OP1 --> HCHO + HO + HO2                                                           ', &
     '                  OP2 --> ALD + HO + HO2                                                            ', &
     '                  PAA --> MO2 + HO                                                                  ', &
     '                  KET --> ACO3 + ETHP                                                               ', &
     '                  GLY --> 6.95307e-31 CO + 6.95307e-31 HCHO                                         ', &
     '                  GLY --> 6.95307e-31 CO + 6.95307e-31 HCHO + 6.95307e-31 HO2                       ', &
     '                 MGLY --> CO + ACO3 + HO2                                                           ', &
     '                  DCB --> TCO3 + HO2                                                                ', &
     '                 ONIT --> 6.95307e-31 KET + 6.95307e-31 ALD + HO2 + NO2                             ', &
     '                 MACR --> CO + HCHO + ACO3 + HO2                                                    ', &
     '                 HKET --> HCHO + ACO3 + HO2                                                         ', &
     '              O3P + M --> O3                                                                        ', &
     '             O3P + O3 --> M                                                                         ', &
     '              O1D + M --> O3P                                                                       ', &
     '            O1D + H2O --> 6.95307e-31 HO                                                            ', &
     '              O3 + HO --> HO2                                                                       ', &
     '             O3 + HO2 --> HO                                                                        ', &
     '             HO + HO2 --> H2O                                                                       ' /)
  CHARACTER(LEN=100), PARAMETER, DIMENSION(30) :: EQN_NAMES_1 = (/ &
     '            H2O2 + HO --> HO2 + H2O                                                                 ', &
     '      6.95307e-31 HO2 --> H2O2                                                                      ', &
     '6.95307e-31 HO2 + H2O --> H2O2 + H2O                                                                ', &
     '             O3P + NO --> NO2                                                                       ', &
     '            O3P + NO2 --> NO                                                                        ', &
     '            O3P + NO2 --> NO3                                                                       ', &
     '              HO + NO --> HONO                                                                      ', &
     '             HO + NO2 --> HNO3                                                                      ', &
     '             HO + NO3 --> HO2 + NO2                                                                 ', &
     '             HO2 + NO --> HO + NO2                                                                  ', &
     '            HO2 + NO2 --> HNO4                                                                      ', &
     '                 HNO4 --> HO2 + NO2                                                                 ', &
     '            HO2 + NO3 --> 6.95307e-31 HNO3 + 6.95307e-31 HO + 6.95307e-31 NO2                       ', &
     '            HONO + HO --> NO2 + H2O                                                                 ', &
     '            HNO3 + HO --> NO3 + H2O                                                                 ', &
     '            HNO4 + HO --> NO2 + H2O                                                                 ', &
     '              O3 + NO --> NO2                                                                       ', &
     '             O3 + NO2 --> NO3                                                                       ', &
     '   6.95307e-31 NO + M --> 6.95307e-31 NO2                                                           ', &
     '             NO + NO3 --> 6.95307e-31 NO2                                                           ', &
     '            NO2 + NO3 --> NO + NO2                                                                  ', &
     '            NO2 + NO3 --> N2O5                                                                      ', &
     '                 N2O5 --> NO2 + NO3                                                                 ', &
     '      6.95307e-31 NO3 --> 6.95307e-31 NO2                                                           ', &
     '               HO + M --> HO2 + H2O                                                                 ', &
     '             SO2 + HO --> SULF + HO2                                                                ', &
     '              CO + HO --> CO2 + HO2                                                                 ', &
     '            NALD + HO --> CO + HCHO + NO2                                                           ', &
     '            HACE + HO --> MGLY + HO2                                                                ', &
     '             CH4 + HO --> MO2 + H2O                                                                 ' /)
  CHARACTER(LEN=100), PARAMETER, DIMENSION(30) :: EQN_NAMES_2 = (/ &
     '             ETH + HO --> ETHP + H2O                                                                ', &
     '             HC3 + HO --> 6.95307e-31 ORA1 + 6.95307e-31 CO + 6.95307e-31 GLY + 6.95307e-31 HCHO + 6', &
     '             HC5 + HO --> 6.95307e-31 KET + 6.95307e-31 HC5P + 6.95307e-31 HO2 + H2O                ', &
     '             HC8 + HO --> 6.95307e-31 HKET + 6.95307e-31 HC8P + 6.95307e-31 ALD + 6.95307e-31 HO2 + ', &
     '             ETE + HO --> ETEP                                                                      ', &
     '             OLT + HO --> OLTP                                                                      ', &
     '             OLI + HO --> OLIP                                                                      ', &
     '            DIEN + HO --> ISOP                                                                      ', &
     '             ISO + HO --> ISOP                                                                      ', &
     '             API + HO --> APIP                                                                      ', &
     '             LIM + HO --> LIMP                                                                      ', &
     '             TOL + HO --> 6.95307e-31 ADDT + 6.95307e-31 XO2 + 6.95307e-31 HO2                      ', &
     '             XYL + HO --> 6.95307e-31 ADDX + 6.95307e-31 XO2 + 6.95307e-31 HO2                      ', &
     '             CSL + HO --> 6.95307e-31 PHO + 6.95307e-31 ADDC + 6.95307e-31 XO2 + 6.95307e-31 HO2    ', &
     '            HCHO + HO --> CO + HO2 + H2O                                                            ', &
     '             ALD + HO --> ACO3 + H2O                                                                ', &
     '             KET + HO --> KETP + H2O                                                                ', &
     '            HKET + HO --> MGLY + HO2 + H2O                                                          ', &
     '             GLY + HO --> 6.95307e-31 CO + HO2 + H2O                                                ', &
     '            MGLY + HO --> CO + ACO3 + H2O                                                           ', &
     '            MACR + HO --> MACP                                                                      ', &
     '             DCB + HO --> 6.95307e-31 UDD + 6.95307e-31 GLY + 6.95307e-31 MGLY + 6.95307e-31 XO2 + 6', &
     '             UDD + HO --> 6.95307e-31 KET + 6.95307e-31 ALD + HO2                                   ', &
     '             OP1 + HO --> 6.95307e-31 HCHO + 6.95307e-31 MO2 + 6.95307e-31 HO                       ', &
     '             HO + OP2 --> 6.95307e-31 KET + 6.95307e-31 XO2 + 6.95307e-31 ALD + 6.95307e-31 HC3P + 6', &
     '             PAA + HO --> 6.95307e-31 HCHO + 6.95307e-31 XO2 + 6.95307e-31 ACO3 + 6.95307e-31 HO2   ', &
     '             PAN + HO --> HCHO + XO2 + NO3 + H2O                                                    ', &
     '            TPAN + HO --> 6.95307e-31 HKET + 6.95307e-31 PAN + 6.95307e-31 HCHO + XO2 + 6.95307e-31 ', &
     '            ONIT + HO --> HC3P + NO2 + H2O                                                          ', &
     '           HCHO + NO3 --> HNO3 + CO + HO2                                                           ' /)
  CHARACTER(LEN=100), PARAMETER, DIMENSION(30) :: EQN_NAMES_3 = (/ &
     '            ALD + NO3 --> HNO3 + ACO3                                                               ', &
     '            GLY + NO3 --> HNO3 + 6.95307e-31 CO + HO2                                               ', &
     '           MGLY + NO3 --> HNO3 + CO + ACO3                                                          ', &
     '            MAHP + HO --> MACP                                                                      ', &
     '            DCB + NO3 --> 6.95307e-31 HNO3 + 6.95307e-31 GLY + 6.95307e-31 KET + 6.95307e-31 MGLY + ', &
     '            CSL + NO3 --> PHO + HNO3                                                                ', &
     '            ETE + NO3 --> 6.95307e-31 OLND + 6.95307e-31 OLNN                                       ', &
     '            OLT + NO3 --> 6.95307e-31 OLND + 6.95307e-31 OLNN                                       ', &
     '            OLI + NO3 --> 6.95307e-31 OLND + 6.95307e-31 OLNN                                       ', &
     '           DIEN + NO3 --> 6.95307e-31 MACR + 6.95307e-31 OLND + 6.95307e-31 OLNN                    ', &
     '            ISO + NO3 --> ISON                                                                      ', &
     '            API + NO3 --> 6.95307e-31 OLND + 6.95307e-31 OLNN                                       ', &
     '            LIM + NO3 --> 6.95307e-31 OLND + 6.95307e-31 OLNN                                       ', &
     '           TPAN + NO3 --> 6.95307e-31 PAN + 6.95307e-31 HCHO + 6.95307e-31 ONIT + XO2 + 6.95307e-31 ', &
     '             ETE + O3 --> 6.95307e-31 ORA1 + 6.95307e-31 CO + HCHO + 6.95307e-31 HO + 6.95307e-31 HO', &
     '             OLT + O3 --> 6.95307e-31 ORA1 + 6.95307e-31 ORA2 + 6.95307e-31 ETH + 6.95307e-31 CH4 + ', &
     '             OLI + O3 --> 6.95307e-31 ORA2 + 6.95307e-31 ETH + 6.95307e-31 CH4 + 6.95307e-31 H2O2 + ', &
     '            DIEN + O3 --> 6.95307e-31 ORA1 + 6.95307e-31 O3P + 6.95307e-31 H2O2 + 6.95307e-31 CO + 6', &
     '             ISO + O3 --> 6.95307e-31 ORA1 + 6.95307e-31 H2O2 + 6.95307e-31 CO + 6.95307e-31 MACP + ', &
     '             API + O3 --> 6.95307e-31 H2O2 + 6.95307e-31 CO + 6.95307e-31 KET + 6.95307e-31 ALD + 6.', &
     '             LIM + O3 --> 6.95307e-31 ORA1 + 6.95307e-31 ORA2 + 6.95307e-31 H2O2 + 6.95307e-31 CO + ', &
     '            MACR + O3 --> 6.95307e-31 ORA1 + 6.95307e-31 CO + 6.95307e-31 MGLY + 6.95307e-31 ACO3 + ', &
     '             DCB + O3 --> 6.95307e-31 ORA1 + 6.95307e-31 ORA2 + 6.95307e-31 PAA + 6.95307e-31 CO + 6', &
     '            TPAN + O3 --> 6.95307e-31 ORA1 + 6.95307e-31 PAN + 6.95307e-31 CO + 6.95307e-31 HCHO + 6', &
     '            PHO + NO2 --> 6.95307e-31 CSL + ONIT                                                    ', &
     '            PHO + HO2 --> CSL                                                                       ', &
     '           ADDT + NO2 --> HONO + CSL                                                                ', &
     '             ADDT + M --> 6.95307e-31 CSL + 6.95307e-31 TOLP + 6.95307e-31 HO2                      ', &
     '            ADDT + O3 --> CSL + HO                                                                  ', &
     '           ADDX + NO2 --> HONO + CSL                                                                ' /)
  CHARACTER(LEN=100), PARAMETER, DIMENSION(30) :: EQN_NAMES_4 = (/ &
     '             ADDX + M --> 6.95307e-31 CSL + 6.95307e-31 XYLP + 6.95307e-31 HO2                      ', &
     '            ADDX + O3 --> CSL + HO                                                                  ', &
     '           ADDC + NO2 --> HONO + CSL                                                                ', &
     '             ADDC + M --> 6.95307e-31 CSL + 6.95307e-31 CSLP + 6.95307e-31 HO2                      ', &
     '            ADDC + O3 --> CSL + HO                                                                  ', &
     '           ACO3 + NO2 --> PAN                                                                       ', &
     '                  PAN --> ACO3 + NO2                                                                ', &
     '           TCO3 + NO2 --> TPAN                                                                      ', &
     '                 TPAN --> TCO3 + NO2                                                                ', &
     '             MO2 + NO --> HCHO + HO2 + NO2                                                          ', &
     '            NO + ETHP --> ALD + HO2 + NO2                                                           ', &
     '            HC3P + NO --> 6.95307e-31 GLY + 6.95307e-31 KET + 6.95307e-31 HCHO + 6.95307e-31 ONIT + ', &
     '            HC5P + NO --> 6.95307e-31 KET + 6.95307e-31 HCHO + 6.95307e-31 ONIT + 6.95307e-31 XO2 + ', &
     '            HC8P + NO --> 6.95307e-31 KET + 6.95307e-31 ONIT + 6.95307e-31 XO2 + 6.95307e-31 ALD + 6', &
     '            ETEP + NO --> 6.95307e-31 HCHO + 6.95307e-31 ALD + HO2 + NO2                            ', &
     '            OLTP + NO --> 6.95307e-31 KET + HCHO + 6.95307e-31 ALD + HO2 + NO2                      ', &
     '            OLIP + NO --> 6.95307e-31 KET + 6.95307e-31 ALD + HO2 + NO2                             ', &
     '            ISOP + NO --> 6.95307e-31 ISON + MACR + HCHO + HO2 + NO2                                ', &
     '            APIP + NO --> 6.95307e-31 KET + 6.95307e-31 ONIT + 6.95307e-31 ALD + 6.95307e-31 HO2 + 6', &
     '            LIMP + NO --> 6.95307e-31 MACR + 6.95307e-31 HCHO + 6.95307e-31 ONIT + 6.95307e-31 OLI +', &
     '            TOLP + NO --> 6.95307e-31 GLY + 6.95307e-31 MGLY + 6.95307e-31 ONIT + 6.95307e-31 DCB + ', &
     '            XYLP + NO --> 6.95307e-31 GLY + 6.95307e-31 MGLY + 6.95307e-31 ONIT + 6.95307e-31 DCB + ', &
     '            CSLP + NO --> GLY + MGLY + HO2 + NO2                                                    ', &
     '            ACO3 + NO --> MO2 + NO2                                                                 ', &
     '            TCO3 + NO --> HCHO + ACO3 + NO2                                                         ', &
     '            KETP + NO --> 6.95307e-31 MGLY + 6.95307e-31 XO2 + 6.95307e-31 ALD + 6.95307e-31 ACO3 + ', &
     '            OLNN + NO --> ONIT + HO2 + NO2                                                          ', &
     '            OLND + NO --> 6.95307e-31 KET + 6.95307e-31 HCHO + 6.95307e-31 ALD + 6.95307e-31 NO2    ', &
     '            MO2 + HO2 --> OP1                                                                       ', &
     '           HO2 + ETHP --> OP2                                                                       ' /)
  CHARACTER(LEN=100), PARAMETER, DIMENSION(30) :: EQN_NAMES_5 = (/ &
     '           HC3P + HO2 --> OP2                                                                       ', &
     '           HC5P + HO2 --> OP2                                                                       ', &
     '           HC8P + HO2 --> OP2                                                                       ', &
     '           ETEP + HO2 --> OP2                                                                       ', &
     '           OLTP + HO2 --> OP2                                                                       ', &
     '           OLIP + HO2 --> OP2                                                                       ', &
     '           ISOP + HO2 --> ISHP                                                                      ', &
     '           APIP + HO2 --> OP2                                                                       ', &
     '           LIMP + HO2 --> OP2                                                                       ', &
     '           TOLP + HO2 --> OP2                                                                       ', &
     '           XYLP + HO2 --> OP2                                                                       ', &
     '           CSLP + HO2 --> OP2                                                                       ', &
     '           ACO3 + HO2 --> PAA                                                                       ', &
     '           ACO3 + HO2 --> ORA2 + O3                                                                 ', &
     '           TCO3 + HO2 --> OP2                                                                       ', &
     '           TCO3 + HO2 --> ORA2 + O3                                                                 ', &
     '           KETP + HO2 --> OP2                                                                       ', &
     '           OLNN + HO2 --> ONIT                                                                      ', &
     '           OLND + HO2 --> ONIT                                                                      ', &
     '      6.95307e-31 MO2 --> 6.95307e-31 HCHO + 6.95307e-31 HO2                                        ', &
     '           MO2 + ETHP --> 6.95307e-31 HCHO + 6.95307e-31 ALD + HO2                                  ', &
     '           HC3P + MO2 --> 6.95307e-31 GLY + 6.95307e-31 KET + 6.95307e-31 MGLY + 6.95307e-31 HCHO + ', &
     '           HC5P + MO2 --> 6.95307e-31 KET + 6.95307e-31 HCHO + 6.95307e-31 XO2 + 6.95307e-31 ALD + 6', &
     '           HC8P + MO2 --> 6.95307e-31 KET + 6.95307e-31 HCHO + 6.95307e-31 XO2 + 6.95307e-31 ALD + 6', &
     '           ETEP + MO2 --> 6.95307e-31 HCHO + 6.95307e-31 ALD + HO2                                  ', &
     '           OLTP + MO2 --> 6.95307e-31 KET + 6.95307e-31 HCHO + 6.95307e-31 ALD + HO2                ', &
     '           OLIP + MO2 --> 6.95307e-31 KET + 6.95307e-31 HCHO + 6.95307e-31 ALD + HO2                ', &
     '           ISOP + MO2 --> 6.95307e-31 MACR + 6.95307e-31 HCHO + 6.95307e-31 OLI + 6.95307e-31 OLT + ', &
     '           APIP + MO2 --> KET + HCHO + ALD + 6.95307e-31 HO2                                        ', &
     '           LIMP + MO2 --> 6.95307e-31 MACR + 6.95307e-31 HCHO + 6.95307e-31 OLI + 6.95307e-31 HO2   ' /)
  CHARACTER(LEN=100), PARAMETER, DIMENSION(30) :: EQN_NAMES_6 = (/ &
     '           TOLP + MO2 --> 6.95307e-31 GLY + 6.95307e-31 MGLY + HCHO + DCB + HO2                     ', &
     '           XYLP + MO2 --> 6.95307e-31 GLY + 6.95307e-31 MGLY + HCHO + DCB + HO2                     ', &
     '           CSLP + MO2 --> GLY + MGLY + HCHO + 6.95307e-31 HO2                                       ', &
     '           ACO3 + MO2 --> HCHO + MO2 + HO2                                                          ', &
     '           ACO3 + MO2 --> ORA2 + HCHO                                                               ', &
     '           TCO3 + MO2 --> 6.95307e-31 HCHO + ACO3 + HO2                                             ', &
     '           TCO3 + MO2 --> ORA2 + HCHO                                                               ', &
     '           KETP + MO2 --> 6.95307e-31 HKET + 6.95307e-31 MGLY + 6.95307e-31 HCHO + 6.95307e-31 XO2 +', &
     '           OLNN + MO2 --> 6.95307e-31 HCHO + ONIT + HO2                                             ', &
     '           OLND + MO2 --> 6.95307e-31 KET + 6.95307e-31 HCHO + 6.95307e-31 ONIT + 6.95307e-31 ALD + ', &
     '          ACO3 + ETHP --> 6.95307e-31 ORA2 + ALD + 6.95307e-31 MO2 + 6.95307e-31 HO2                ', &
     '          HC3P + ACO3 --> 6.95307e-31 ORA2 + 6.95307e-31 GLY + 6.95307e-31 KET + 6.95307e-31 MGLY + ', &
     '          HC5P + ACO3 --> 6.95307e-31 ORA2 + 6.95307e-31 KET + 6.95307e-31 HCHO + 6.95307e-31 XO2 + ', &
     '          HC8P + ACO3 --> 6.95307e-31 ORA2 + 6.95307e-31 KET + 6.95307e-31 XO2 + 6.95307e-31 ALD + 6', &
     '          ETEP + ACO3 --> 6.95307e-31 ORA2 + 6.95307e-31 HCHO + 6.95307e-31 ALD + 6.95307e-31 MO2 + ', &
     '          OLTP + ACO3 --> 6.95307e-31 ORA2 + 6.95307e-31 KET + 6.95307e-31 HCHO + 6.95307e-31 ALD + ', &
     '          OLIP + ACO3 --> 6.95307e-31 ORA2 + 6.95307e-31 KET + 6.95307e-31 ALD + 6.95307e-31 MO2 + 6', &
     '          ISOP + ACO3 --> 6.95307e-31 ORA2 + 6.95307e-31 MACR + 6.95307e-31 HCHO + 6.95307e-31 OLT +', &
     '          APIP + ACO3 --> KET + ALD + MO2 + HO2                                                     ', &
     '          LIMP + ACO3 --> 6.95307e-31 MACR + 6.95307e-31 HCHO + 6.95307e-31 OLI + MO2 + HO2         ', &
     '          TOLP + ACO3 --> 6.95307e-31 GLY + 6.95307e-31 MGLY + DCB + MO2 + HO2                      ', &
     '          XYLP + ACO3 --> 6.95307e-31 GLY + 6.95307e-31 MGLY + DCB + MO2 + HO2                      ', &
     '          CSLP + ACO3 --> GLY + MGLY + MO2 + HO2                                                    ', &
     '     6.95307e-31 ACO3 --> 6.95307e-31 MO2                                                           ', &
     '          TCO3 + ACO3 --> HCHO + ACO3 + MO2                                                         ', &
     '          KETP + ACO3 --> 6.95307e-31 ORA2 + 6.95307e-31 KET + 6.95307e-31 MGLY + 6.95307e-31 XO2 + ', &
     '          OLNN + ACO3 --> 6.95307e-31 ORA2 + ONIT + 6.95307e-31 MO2 + 6.95307e-31 HO2               ', &
     '          OLND + ACO3 --> 6.95307e-31 ORA2 + 6.95307e-31 KET + 6.95307e-31 HCHO + 6.95307e-31 ONIT +', &
     '     6.95307e-31 OLNN --> 6.95307e-31 ONIT + HO2                                                    ', &
     '          OLND + OLNN --> 6.95307e-31 KET + 6.95307e-31 HCHO + 6.95307e-31 ONIT + 6.95307e-31 ALD + ' /)
  CHARACTER(LEN=100), PARAMETER, DIMENSION(30) :: EQN_NAMES_7 = (/ &
     '     6.95307e-31 OLND --> 6.95307e-31 KET + 6.95307e-31 HCHO + ONIT + 6.95307e-31 ALD + NO2         ', &
     '            MO2 + NO3 --> HCHO + HO2 + NO2                                                          ', &
     '           NO3 + ETHP --> ALD + HO2 + NO2                                                           ', &
     '           HC3P + NO3 --> 6.95307e-31 GLY + 6.95307e-31 KET + 6.95307e-31 HCHO + 6.95307e-31 XO2 + 6', &
     '           HC5P + NO3 --> 6.95307e-31 KET + 6.95307e-31 HCHO + 6.95307e-31 XO2 + 6.95307e-31 ALD + 6', &
     '           HC8P + NO3 --> 6.95307e-31 KET + 6.95307e-31 XO2 + 6.95307e-31 ALD + 6.95307e-31 HO2 + NO', &
     '           ETEP + NO3 --> 6.95307e-31 HCHO + 6.95307e-31 ALD + HO2 + NO2                            ', &
     '           OLTP + NO3 --> 6.95307e-31 KET + HCHO + 6.95307e-31 ALD + HO2 + NO2                      ', &
     '           OLIP + NO3 --> 6.95307e-31 KET + 6.95307e-31 ALD + HO2 + NO2                             ', &
     '            MPAN + HO --> HACE + NO2                                                                ', &
     '           APIP + NO3 --> KET + ALD + HO2 + NO2                                                     ', &
     '           LIMP + NO3 --> 6.95307e-31 MACR + 6.95307e-31 HCHO + 6.95307e-31 OLI + HO2 + NO2         ', &
     '           TOLP + NO3 --> 6.95307e-31 GLY + 6.95307e-31 MGLY + 6.95307e-31 DCB + HO2 + NO2          ', &
     '           XYLP + NO3 --> 6.95307e-31 GLY + 6.95307e-31 MGLY + DCB + HO2 + NO2                      ', &
     '           CSLP + NO3 --> GLY + MGLY + HO2 + NO2                                                    ', &
     '           ACO3 + NO3 --> MO2 + NO2                                                                 ', &
     '           TCO3 + NO3 --> HCHO + ACO3 + NO2                                                         ', &
     '           KETP + NO3 --> 6.95307e-31 MGLY + 6.95307e-31 XO2 + 6.95307e-31 ALD + 6.95307e-31 ACO3 + ', &
     '           OLNN + NO3 --> ONIT + HO2 + NO2                                                          ', &
     '           OLND + NO3 --> 6.95307e-31 KET + 6.95307e-31 HCHO + 6.95307e-31 ALD + 6.95307e-31 NO2    ', &
     '            XO2 + HO2 --> OP2                                                                       ', &
     '            XO2 + MO2 --> HCHO + HO2                                                                ', &
     '           XO2 + ACO3 --> MO2                                                                       ', &
     '      6.95307e-31 XO2 --> M                                                                         ', &
     '             XO2 + NO --> NO2                                                                       ', &
     '            XO2 + NO3 --> NO2                                                                       ', &
     '     6.95307e-31 ISOP --> 6.95307e-31 MACR + HCHO + HO2                                             ', &
     '            ISHP + HO --> MACR + HO                                                                 ', &
     '            ISON + HO --> NALD + HACE                                                               ', &
     '            MACP + NO --> 6.95307e-31 HACE + 6.95307e-31 CO + 6.95307e-31 MGLY + 6.95307e-31 HCHO + ' /)
  CHARACTER(LEN=100), PARAMETER, DIMENSION(16) :: EQN_NAMES_8 = (/ &
     '           MACP + HO2 --> MAHP                                                                      ', &
     '     6.95307e-31 MACP --> HACE + 6.95307e-31 CO + MGLY + 6.95307e-31 HCHO + HO2                     ', &
     '           MACP + NO2 --> MPAN                                                                      ', &
     '                 MPAN --> MACP + NO2                                                                ', &
     '            SESQ + HO --> 6.95307e-31 ORA1 + 6.95307e-31 KET + 6.95307e-31 HCHO + 6.95307e-31 OLIP  ', &
     '            SESQ + O3 --> 6.95307e-31 ORA1 + 6.95307e-31 ORA2 + 6.95307e-31 KET + 6.95307e-31 HCHO +', &
     '           SESQ + NO3 --> 6.95307e-31 MACR + 6.95307e-31 OLND + 6.95307e-31 OLNN                    ', &
     '             MBO + HO --> OLIP                                                                      ', &
     '            MBO + NO3 --> 6.95307e-31 OLND + 6.95307e-31 OLNN                                       ', &
     '             MBO + O3 --> 6.95307e-31 ORA2 + 6.95307e-31 ETH + 6.95307e-31 CH4 + 6.95307e-31 H2O2 + ', &
     '         CVASOA4 + HO --> 6.95307e-31 CVASOA3 + HO                                                  ', &
     '         CVASOA3 + HO --> 6.95307e-31 CVASOA2 + HO                                                  ', &
     '         CVASOA2 + HO --> 6.95307e-31 CVASOA1 + HO                                                  ', &
     '         CVBSOA4 + HO --> 6.95307e-31 CVBSOA3 + HO                                                  ', &
     '         CVBSOA3 + HO --> 6.95307e-31 CVBSOA2 + HO                                                  ', &
     '         CVBSOA2 + HO --> 6.95307e-31 CVBSOA1 + HO                                                  ' /)
  CHARACTER(LEN=100), PARAMETER, DIMENSION(256) :: EQN_NAMES = (/&
    EQN_NAMES_0, EQN_NAMES_1, EQN_NAMES_2, EQN_NAMES_3, EQN_NAMES_4, &
    EQN_NAMES_5, EQN_NAMES_6, EQN_NAMES_7, EQN_NAMES_8 /)

! INLINED global variables

! End INLINED global variables


END MODULE racm_soa_vbs_Monitor
