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
! File                 : racm_soa_vbs_Parameters.f90
! Time                 : Mon Sep 26 23:37:38 2022
! Working directory    : /home/ess/xiaodong/WORK/WRFCHEM/model/WRF4.4/chem/KPP/mechanisms/racm_soa_vbs
! Equation file        : racm_soa_vbs.kpp
! Output root filename : racm_soa_vbs
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



MODULE racm_soa_vbs_Parameters

  USE racm_soa_vbs_Precision
  PUBLIC
  SAVE


! NSPEC - Number of chemical species
  INTEGER, PARAMETER :: NSPEC = 92 
! NVAR - Number of Variable species
  INTEGER, PARAMETER :: NVAR = 90 
! NVARACT - Number of Active species
  INTEGER, PARAMETER :: NVARACT = 84 
! NFIX - Number of Fixed species
  INTEGER, PARAMETER :: NFIX = 2 
! NREACT - Number of reactions
  INTEGER, PARAMETER :: NREACT = 256 
! NVARST - Starting of variables in conc. vect.
  INTEGER, PARAMETER :: NVARST = 1 
! NFIXST - Starting of fixed in conc. vect.
  INTEGER, PARAMETER :: NFIXST = 91 
! NONZERO - Number of nonzero entries in Jacobian
  INTEGER, PARAMETER :: NONZERO = 1009 
! LU_NONZERO - Number of nonzero entries in LU factoriz. of Jacobian
  INTEGER, PARAMETER :: LU_NONZERO = 1147 
! CNVAR - (NVAR+1) Number of elements in compressed row format
  INTEGER, PARAMETER :: CNVAR = 91 
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
  INTEGER, PARAMETER :: ind_CVASOA1 = 5 
  INTEGER, PARAMETER :: ind_CVASOA2 = 6 
  INTEGER, PARAMETER :: ind_CVASOA3 = 7 
  INTEGER, PARAMETER :: ind_CVASOA4 = 8 
  INTEGER, PARAMETER :: ind_CVBSOA1 = 9 
  INTEGER, PARAMETER :: ind_CVBSOA2 = 10 
  INTEGER, PARAMETER :: ind_CVBSOA3 = 11 
  INTEGER, PARAMETER :: ind_CVBSOA4 = 12 
  INTEGER, PARAMETER :: ind_O1D = 13 
  INTEGER, PARAMETER :: ind_SO2 = 14 
  INTEGER, PARAMETER :: ind_ISHP = 15 
  INTEGER, PARAMETER :: ind_TOL = 16 
  INTEGER, PARAMETER :: ind_XYL = 17 
  INTEGER, PARAMETER :: ind_HC5 = 18 
  INTEGER, PARAMETER :: ind_N2O5 = 19 
  INTEGER, PARAMETER :: ind_HC8 = 20 
  INTEGER, PARAMETER :: ind_MAHP = 21 
  INTEGER, PARAMETER :: ind_HC3 = 22 
  INTEGER, PARAMETER :: ind_UDD = 23 
  INTEGER, PARAMETER :: ind_NALD = 24 
  INTEGER, PARAMETER :: ind_ETH = 25 
  INTEGER, PARAMETER :: ind_CH4 = 26 
  INTEGER, PARAMETER :: ind_OP1 = 27 
  INTEGER, PARAMETER :: ind_MPAN = 28 
  INTEGER, PARAMETER :: ind_HNO4 = 29 
  INTEGER, PARAMETER :: ind_HACE = 30 
  INTEGER, PARAMETER :: ind_HONO = 31 
  INTEGER, PARAMETER :: ind_HKET = 32 
  INTEGER, PARAMETER :: ind_O3P = 33 
  INTEGER, PARAMETER :: ind_PHO = 34 
  INTEGER, PARAMETER :: ind_H2O2 = 35 
  INTEGER, PARAMETER :: ind_ADDT = 36 
  INTEGER, PARAMETER :: ind_ADDX = 37 
  INTEGER, PARAMETER :: ind_ETE = 38 
  INTEGER, PARAMETER :: ind_ADDC = 39 
  INTEGER, PARAMETER :: ind_PAA = 40 
  INTEGER, PARAMETER :: ind_ISON = 41 
  INTEGER, PARAMETER :: ind_SESQ = 42 
  INTEGER, PARAMETER :: ind_HNO3 = 43 
  INTEGER, PARAMETER :: ind_PAN = 44 
  INTEGER, PARAMETER :: ind_API = 45 
  INTEGER, PARAMETER :: ind_CO = 46 
  INTEGER, PARAMETER :: ind_LIM = 47 
  INTEGER, PARAMETER :: ind_ISO = 48 
  INTEGER, PARAMETER :: ind_MBO = 49 
  INTEGER, PARAMETER :: ind_CSL = 50 
  INTEGER, PARAMETER :: ind_DIEN = 51 
  INTEGER, PARAMETER :: ind_MACP = 52 
  INTEGER, PARAMETER :: ind_GLY = 53 
  INTEGER, PARAMETER :: ind_TPAN = 54 
  INTEGER, PARAMETER :: ind_ETEP = 55 
  INTEGER, PARAMETER :: ind_OLTP = 56 
  INTEGER, PARAMETER :: ind_MACR = 57 
  INTEGER, PARAMETER :: ind_APIP = 58 
  INTEGER, PARAMETER :: ind_KET = 59 
  INTEGER, PARAMETER :: ind_ISOP = 60 
  INTEGER, PARAMETER :: ind_CSLP = 61 
  INTEGER, PARAMETER :: ind_MGLY = 62 
  INTEGER, PARAMETER :: ind_LIMP = 63 
  INTEGER, PARAMETER :: ind_HC5P = 64 
  INTEGER, PARAMETER :: ind_HC8P = 65 
  INTEGER, PARAMETER :: ind_HCHO = 66 
  INTEGER, PARAMETER :: ind_TOLP = 67 
  INTEGER, PARAMETER :: ind_XYLP = 68 
  INTEGER, PARAMETER :: ind_OLIP = 69 
  INTEGER, PARAMETER :: ind_ONIT = 70 
  INTEGER, PARAMETER :: ind_DCB = 71 
  INTEGER, PARAMETER :: ind_XO2 = 72 
  INTEGER, PARAMETER :: ind_OLI = 73 
  INTEGER, PARAMETER :: ind_ALD = 74 
  INTEGER, PARAMETER :: ind_OLT = 75 
  INTEGER, PARAMETER :: ind_OLND = 76 
  INTEGER, PARAMETER :: ind_OLNN = 77 
  INTEGER, PARAMETER :: ind_TCO3 = 78 
  INTEGER, PARAMETER :: ind_O3 = 79 
  INTEGER, PARAMETER :: ind_HC3P = 80 
  INTEGER, PARAMETER :: ind_KETP = 81 
  INTEGER, PARAMETER :: ind_ACO3 = 82 
  INTEGER, PARAMETER :: ind_MO2 = 83 
  INTEGER, PARAMETER :: ind_HO = 84 
  INTEGER, PARAMETER :: ind_HO2 = 85 
  INTEGER, PARAMETER :: ind_OP2 = 86 
  INTEGER, PARAMETER :: ind_NO = 87 
  INTEGER, PARAMETER :: ind_NO2 = 88 
  INTEGER, PARAMETER :: ind_NO3 = 89 
  INTEGER, PARAMETER :: ind_ETHP = 90 

! Index declaration for fixed species in C
!   C(ind_spc)

  INTEGER, PARAMETER :: ind_H2O = 91 
  INTEGER, PARAMETER :: ind_M = 92 

! Index declaration for fixed species in FIX
!    FIX(indf_spc) = C(ind_spc) = C(NVAR+indf_spc)

  INTEGER, PARAMETER :: indf_H2O = 1 
  INTEGER, PARAMETER :: indf_M = 2 

END MODULE racm_soa_vbs_Parameters

