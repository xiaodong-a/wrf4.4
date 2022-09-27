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
! File                 : racmpm_Parameters.f90
! Time                 : Mon Sep 26 23:37:29 2022
! Working directory    : /home/ess/xiaodong/WORK/WRFCHEM/model/WRF4.4/chem/KPP/mechanisms/racmpm
! Equation file        : racmpm.kpp
! Output root filename : racmpm
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



MODULE racmpm_Parameters

  USE racmpm_Precision
  PUBLIC
  SAVE


! NSPEC - Number of chemical species
  INTEGER, PARAMETER :: NSPEC = 75 
! NVAR - Number of Variable species
  INTEGER, PARAMETER :: NVAR = 73 
! NVARACT - Number of Active species
  INTEGER, PARAMETER :: NVARACT = 69 
! NFIX - Number of Fixed species
  INTEGER, PARAMETER :: NFIX = 2 
! NREACT - Number of reactions
  INTEGER, PARAMETER :: NREACT = 237 
! NVARST - Starting of variables in conc. vect.
  INTEGER, PARAMETER :: NVARST = 1 
! NFIXST - Starting of fixed in conc. vect.
  INTEGER, PARAMETER :: NFIXST = 74 
! NONZERO - Number of nonzero entries in Jacobian
  INTEGER, PARAMETER :: NONZERO = 926 
! LU_NONZERO - Number of nonzero entries in LU factoriz. of Jacobian
  INTEGER, PARAMETER :: LU_NONZERO = 1052 
! CNVAR - (NVAR+1) Number of elements in compressed row format
  INTEGER, PARAMETER :: CNVAR = 74 
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

  INTEGER, PARAMETER :: ind_SULF = 1 
  INTEGER, PARAMETER :: ind_CO2 = 2 
  INTEGER, PARAMETER :: ind_ORA1 = 3 
  INTEGER, PARAMETER :: ind_ORA2 = 4 
  INTEGER, PARAMETER :: ind_SO2 = 5 
  INTEGER, PARAMETER :: ind_O1D = 6 
  INTEGER, PARAMETER :: ind_HC5 = 7 
  INTEGER, PARAMETER :: ind_TOL = 8 
  INTEGER, PARAMETER :: ind_XYL = 9 
  INTEGER, PARAMETER :: ind_N2O5 = 10 
  INTEGER, PARAMETER :: ind_HC8 = 11 
  INTEGER, PARAMETER :: ind_HC3 = 12 
  INTEGER, PARAMETER :: ind_ETH = 13 
  INTEGER, PARAMETER :: ind_CH4 = 14 
  INTEGER, PARAMETER :: ind_UDD = 15 
  INTEGER, PARAMETER :: ind_HNO4 = 16 
  INTEGER, PARAMETER :: ind_OP1 = 17 
  INTEGER, PARAMETER :: ind_HONO = 18 
  INTEGER, PARAMETER :: ind_H2O2 = 19 
  INTEGER, PARAMETER :: ind_PHO = 20 
  INTEGER, PARAMETER :: ind_ADDT = 21 
  INTEGER, PARAMETER :: ind_ADDX = 22 
  INTEGER, PARAMETER :: ind_HKET = 23 
  INTEGER, PARAMETER :: ind_ETE = 24 
  INTEGER, PARAMETER :: ind_ADDC = 25 
  INTEGER, PARAMETER :: ind_PAA = 26 
  INTEGER, PARAMETER :: ind_HNO3 = 27 
  INTEGER, PARAMETER :: ind_CO = 28 
  INTEGER, PARAMETER :: ind_API = 29 
  INTEGER, PARAMETER :: ind_LIM = 30 
  INTEGER, PARAMETER :: ind_PAN = 31 
  INTEGER, PARAMETER :: ind_CSL = 32 
  INTEGER, PARAMETER :: ind_DIEN = 33 
  INTEGER, PARAMETER :: ind_GLY = 34 
  INTEGER, PARAMETER :: ind_TPAN = 35 
  INTEGER, PARAMETER :: ind_ETEP = 36 
  INTEGER, PARAMETER :: ind_ISO = 37 
  INTEGER, PARAMETER :: ind_OLTP = 38 
  INTEGER, PARAMETER :: ind_OLIP = 39 
  INTEGER, PARAMETER :: ind_MGLY = 40 
  INTEGER, PARAMETER :: ind_CSLP = 41 
  INTEGER, PARAMETER :: ind_KET = 42 
  INTEGER, PARAMETER :: ind_LIMP = 43 
  INTEGER, PARAMETER :: ind_HC5P = 44 
  INTEGER, PARAMETER :: ind_HC8P = 45 
  INTEGER, PARAMETER :: ind_TOLP = 46 
  INTEGER, PARAMETER :: ind_XYLP = 47 
  INTEGER, PARAMETER :: ind_HCHO = 48 
  INTEGER, PARAMETER :: ind_APIP = 49 
  INTEGER, PARAMETER :: ind_ISOP = 50 
  INTEGER, PARAMETER :: ind_MACR = 51 
  INTEGER, PARAMETER :: ind_HC3P = 52 
  INTEGER, PARAMETER :: ind_ALD = 53 
  INTEGER, PARAMETER :: ind_DCB = 54 
  INTEGER, PARAMETER :: ind_TCO3 = 55 
  INTEGER, PARAMETER :: ind_XO2 = 56 
  INTEGER, PARAMETER :: ind_OLT = 57 
  INTEGER, PARAMETER :: ind_OLI = 58 
  INTEGER, PARAMETER :: ind_OLNN = 59 
  INTEGER, PARAMETER :: ind_OLND = 60 
  INTEGER, PARAMETER :: ind_ETHP = 61 
  INTEGER, PARAMETER :: ind_O3P = 62 
  INTEGER, PARAMETER :: ind_O3 = 63 
  INTEGER, PARAMETER :: ind_KETP = 64 
  INTEGER, PARAMETER :: ind_MO2 = 65 
  INTEGER, PARAMETER :: ind_ACO3 = 66 
  INTEGER, PARAMETER :: ind_HO2 = 67 
  INTEGER, PARAMETER :: ind_HO = 68 
  INTEGER, PARAMETER :: ind_ONIT = 69 
  INTEGER, PARAMETER :: ind_NO3 = 70 
  INTEGER, PARAMETER :: ind_OP2 = 71 
  INTEGER, PARAMETER :: ind_NO = 72 
  INTEGER, PARAMETER :: ind_NO2 = 73 

! Index declaration for fixed species in C
!   C(ind_spc)

  INTEGER, PARAMETER :: ind_H2O = 74 
  INTEGER, PARAMETER :: ind_M = 75 

! Index declaration for fixed species in FIX
!    FIX(indf_spc) = C(ind_spc) = C(NVAR+indf_spc)

  INTEGER, PARAMETER :: indf_H2O = 1 
  INTEGER, PARAMETER :: indf_M = 2 

END MODULE racmpm_Parameters

