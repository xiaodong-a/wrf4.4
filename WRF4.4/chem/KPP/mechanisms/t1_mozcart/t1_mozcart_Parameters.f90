! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! Parameter Module File
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
! File                 : t1_mozcart_Parameters.f90
! Time                 : Mon Sep 26 23:37:56 2022
! Working directory    : /home/ess/xiaodong/WORK/WRFCHEM/model/WRF4.4/chem/KPP/mechanisms/t1_mozcart
! Equation file        : t1_mozcart.kpp
! Output root filename : t1_mozcart
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



MODULE t1_mozcart_Parameters

  USE t1_mozcart_Precision
  PUBLIC
  SAVE


! NSPEC - Number of chemical species
  INTEGER, PARAMETER :: NSPEC = 147 
! NVAR - Number of Variable species
  INTEGER, PARAMETER :: NVAR = 142 
! NVARACT - Number of Active species
  INTEGER, PARAMETER :: NVARACT = 141 
! NFIX - Number of Fixed species
  INTEGER, PARAMETER :: NFIX = 5 
! NREACT - Number of reactions
  INTEGER, PARAMETER :: NREACT = 344 
! NVARST - Starting of variables in conc. vect.
  INTEGER, PARAMETER :: NVARST = 1 
! NFIXST - Starting of fixed in conc. vect.
  INTEGER, PARAMETER :: NFIXST = 143 
! NONZERO - Number of nonzero entries in Jacobian
  INTEGER, PARAMETER :: NONZERO = 1404 
! LU_NONZERO - Number of nonzero entries in LU factoriz. of Jacobian
  INTEGER, PARAMETER :: LU_NONZERO = 1622 
! CNVAR - (NVAR+1) Number of elements in compressed row format
  INTEGER, PARAMETER :: CNVAR = 143 
! NLOOKAT - Number of species to look at
  INTEGER, PARAMETER :: NLOOKAT = 0 
! NMONITOR - Number of species to monitor
  INTEGER, PARAMETER :: NMONITOR = 0 
! NMASS - Number of atoms to check mass balance
  INTEGER, PARAMETER :: NMASS = 1 
! PI - Value of pi
  REAL(kind=dp), PARAMETER :: PI = 3.14159265358979 

! Index declaration for variable species in C and VAR
!   VAR(ind_spc) = C(ind_spc)

  INTEGER, PARAMETER :: ind_SO4 = 1 
  INTEGER, PARAMETER :: ind_NH3 = 2 
  INTEGER, PARAMETER :: ind_C2H6 = 3 
  INTEGER, PARAMETER :: ind_C3H8 = 4 
  INTEGER, PARAMETER :: ind_CH3CN = 5 
  INTEGER, PARAMETER :: ind_N2O = 6 
  INTEGER, PARAMETER :: ind_SO2 = 7 
  INTEGER, PARAMETER :: ind_EOOH = 8 
  INTEGER, PARAMETER :: ind_IEPOX = 9 
  INTEGER, PARAMETER :: ind_PBZNIT = 10 
  INTEGER, PARAMETER :: ind_C2H2 = 11 
  INTEGER, PARAMETER :: ind_BENZENE = 12 
  INTEGER, PARAMETER :: ind_BEPOMUC = 13 
  INTEGER, PARAMETER :: ind_PHENOL = 14 
  INTEGER, PARAMETER :: ind_HCN = 15 
  INTEGER, PARAMETER :: ind_N2O5 = 16 
  INTEGER, PARAMETER :: ind_TOLUENE = 17 
  INTEGER, PARAMETER :: ind_CRESOL = 18 
  INTEGER, PARAMETER :: ind_TEPOMUC = 19 
  INTEGER, PARAMETER :: ind_XOOH = 20 
  INTEGER, PARAMETER :: ind_XYLENES = 21 
  INTEGER, PARAMETER :: ind_XYLOL = 22 
  INTEGER, PARAMETER :: ind_DMS = 23 
  INTEGER, PARAMETER :: ind_HPALD = 24 
  INTEGER, PARAMETER :: ind_BZALD = 25 
  INTEGER, PARAMETER :: ind_ONITR = 26 
  INTEGER, PARAMETER :: ind_H2O2 = 27 
  INTEGER, PARAMETER :: ind_C6H5OOH = 28 
  INTEGER, PARAMETER :: ind_C2H5OH = 29 
  INTEGER, PARAMETER :: ind_EO = 30 
  INTEGER, PARAMETER :: ind_BIGALD2 = 31 
  INTEGER, PARAMETER :: ind_CH3OOH = 32 
  INTEGER, PARAMETER :: ind_HO2NO2 = 33 
  INTEGER, PARAMETER :: ind_C3H7OOH = 34 
  INTEGER, PARAMETER :: ind_HYDRALD = 35 
  INTEGER, PARAMETER :: ind_MACROOH = 36 
  INTEGER, PARAMETER :: ind_MEKOOH = 37 
  INTEGER, PARAMETER :: ind_PHENOOH = 38 
  INTEGER, PARAMETER :: ind_ROOH = 39 
  INTEGER, PARAMETER :: ind_BZOOH = 40 
  INTEGER, PARAMETER :: ind_BIGALD4 = 41 
  INTEGER, PARAMETER :: ind_C2H5OOH = 42 
  INTEGER, PARAMETER :: ind_CH3COOH = 43 
  INTEGER, PARAMETER :: ind_BIGENE = 44 
  INTEGER, PARAMETER :: ind_BIGALK = 45 
  INTEGER, PARAMETER :: ind_C2H4 = 46 
  INTEGER, PARAMETER :: ind_ISOPNOOH = 47 
  INTEGER, PARAMETER :: ind_BENZOOH = 48 
  INTEGER, PARAMETER :: ind_NC4CH2OH = 49 
  INTEGER, PARAMETER :: ind_NTERPOOH = 50 
  INTEGER, PARAMETER :: ind_XYLOLOOH = 51 
  INTEGER, PARAMETER :: ind_BZOO = 52 
  INTEGER, PARAMETER :: ind_HOCH2OO = 53 
  INTEGER, PARAMETER :: ind_PHENO2 = 54 
  INTEGER, PARAMETER :: ind_BIGALD1 = 55 
  INTEGER, PARAMETER :: ind_CH3COOOH = 56 
  INTEGER, PARAMETER :: ind_PAN = 57 
  INTEGER, PARAMETER :: ind_PHENO = 58 
  INTEGER, PARAMETER :: ind_POOH = 59 
  INTEGER, PARAMETER :: ind_TERPOOH = 60 
  INTEGER, PARAMETER :: ind_BENZO2 = 61 
  INTEGER, PARAMETER :: ind_XYLOLO2 = 62 
  INTEGER, PARAMETER :: ind_MEK = 63 
  INTEGER, PARAMETER :: ind_HMPROPO2 = 64 
  INTEGER, PARAMETER :: ind_ACBZO2 = 65 
  INTEGER, PARAMETER :: ind_H2 = 66 
  INTEGER, PARAMETER :: ind_MBOOOH = 67 
  INTEGER, PARAMETER :: ind_MPAN = 68 
  INTEGER, PARAMETER :: ind_ISOPNITB = 69 
  INTEGER, PARAMETER :: ind_ENEO2 = 70 
  INTEGER, PARAMETER :: ind_CH4 = 71 
  INTEGER, PARAMETER :: ind_ISOPNITA = 72 
  INTEGER, PARAMETER :: ind_BIGALD3 = 73 
  INTEGER, PARAMETER :: ind_TERP2OOH = 74 
  INTEGER, PARAMETER :: ind_EO2 = 75 
  INTEGER, PARAMETER :: ind_TERPNIT = 76 
  INTEGER, PARAMETER :: ind_C6H5O2 = 77 
  INTEGER, PARAMETER :: ind_ALKOOH = 78 
  INTEGER, PARAMETER :: ind_TOLOOH = 79 
  INTEGER, PARAMETER :: ind_ALKNIT = 80 
  INTEGER, PARAMETER :: ind_MEKO2 = 81 
  INTEGER, PARAMETER :: ind_PO2 = 82 
  INTEGER, PARAMETER :: ind_HCOOH = 83 
  INTEGER, PARAMETER :: ind_BIGALD = 84 
  INTEGER, PARAMETER :: ind_XYLENOOH = 85 
  INTEGER, PARAMETER :: ind_MALO2 = 86 
  INTEGER, PARAMETER :: ind_TOLO2 = 87 
  INTEGER, PARAMETER :: ind_ISOPOOH = 88 
  INTEGER, PARAMETER :: ind_MBO = 89 
  INTEGER, PARAMETER :: ind_XYLENO2 = 90 
  INTEGER, PARAMETER :: ind_DICARBO2 = 91 
  INTEGER, PARAMETER :: ind_C3H7O2 = 92 
  INTEGER, PARAMETER :: ind_O1D = 93 
  INTEGER, PARAMETER :: ind_CH3OH = 94 
  INTEGER, PARAMETER :: ind_C2H5O2 = 95 
  INTEGER, PARAMETER :: ind_MBONO3O2 = 96 
  INTEGER, PARAMETER :: ind_HMPROP = 97 
  INTEGER, PARAMETER :: ind_ISOP = 98 
  INTEGER, PARAMETER :: ind_NOA = 99 
  INTEGER, PARAMETER :: ind_BCARY = 100 
  INTEGER, PARAMETER :: ind_APIN = 101 
  INTEGER, PARAMETER :: ind_GLYOXAL = 102 
  INTEGER, PARAMETER :: ind_MDIALO2 = 103 
  INTEGER, PARAMETER :: ind_BPIN = 104 
  INTEGER, PARAMETER :: ind_MYRC = 105 
  INTEGER, PARAMETER :: ind_LIMON = 106 
  INTEGER, PARAMETER :: ind_ALKO2 = 107 
  INTEGER, PARAMETER :: ind_CH3COCH3 = 108 
  INTEGER, PARAMETER :: ind_CO = 109 
  INTEGER, PARAMETER :: ind_GLYALD = 110 
  INTEGER, PARAMETER :: ind_MBOO2 = 111 
  INTEGER, PARAMETER :: ind_TERPROD2 = 112 
  INTEGER, PARAMETER :: ind_HNO3 = 113 
  INTEGER, PARAMETER :: ind_C3H6 = 114 
  INTEGER, PARAMETER :: ind_CH3CHO = 115 
  INTEGER, PARAMETER :: ind_TERP2O2 = 116 
  INTEGER, PARAMETER :: ind_NC4CHO = 117 
  INTEGER, PARAMETER :: ind_NTERPO2 = 118 
  INTEGER, PARAMETER :: ind_TERPO2 = 119 
  INTEGER, PARAMETER :: ind_TERPROD1 = 120 
  INTEGER, PARAMETER :: ind_RO2 = 121 
  INTEGER, PARAMETER :: ind_O = 122 
  INTEGER, PARAMETER :: ind_HYAC = 123 
  INTEGER, PARAMETER :: ind_HONITR = 124 
  INTEGER, PARAMETER :: ind_MACRO2 = 125 
  INTEGER, PARAMETER :: ind_CH3COCHO = 126 
  INTEGER, PARAMETER :: ind_XO2 = 127 
  INTEGER, PARAMETER :: ind_ISOPAO2 = 128 
  INTEGER, PARAMETER :: ind_ISOPNO3 = 129 
  INTEGER, PARAMETER :: ind_ISOPBO2 = 130 
  INTEGER, PARAMETER :: ind_MACR = 131 
  INTEGER, PARAMETER :: ind_CH2O = 132 
  INTEGER, PARAMETER :: ind_MVK = 133 
  INTEGER, PARAMETER :: ind_NO3 = 134 
  INTEGER, PARAMETER :: ind_O3 = 135 
  INTEGER, PARAMETER :: ind_CH3CO3 = 136 
  INTEGER, PARAMETER :: ind_NO2 = 137 
  INTEGER, PARAMETER :: ind_OH = 138 
  INTEGER, PARAMETER :: ind_MCO3 = 139 
  INTEGER, PARAMETER :: ind_CH3O2 = 140 
  INTEGER, PARAMETER :: ind_HO2 = 141 
  INTEGER, PARAMETER :: ind_NO = 142 

! Index declaration for fixed species in C
!   C(ind_spc)

  INTEGER, PARAMETER :: ind_M = 143 
  INTEGER, PARAMETER :: ind_H2O = 144 
  INTEGER, PARAMETER :: ind_O2 = 145 
  INTEGER, PARAMETER :: ind_N2 = 146 
  INTEGER, PARAMETER :: ind_CO2 = 147 

! Index declaration for fixed species in FIX
!    FIX(indf_spc) = C(ind_spc) = C(NVAR+indf_spc)

  INTEGER, PARAMETER :: indf_M = 1 
  INTEGER, PARAMETER :: indf_H2O = 2 
  INTEGER, PARAMETER :: indf_O2 = 3 
  INTEGER, PARAMETER :: indf_N2 = 4 
  INTEGER, PARAMETER :: indf_CO2 = 5 

END MODULE t1_mozcart_Parameters

