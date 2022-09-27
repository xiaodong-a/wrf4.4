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
! File                 : cbmz_mosaic_Parameters.f90
! Time                 : Mon Sep 26 23:36:58 2022
! Working directory    : /home/ess/xiaodong/WORK/WRFCHEM/model/WRF4.4/chem/KPP/mechanisms/cbmz_mosaic
! Equation file        : cbmz_mosaic.kpp
! Output root filename : cbmz_mosaic
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



MODULE cbmz_mosaic_Parameters

  USE cbmz_mosaic_Precision
  PUBLIC
  SAVE


! NSPEC - Number of chemical species
  INTEGER, PARAMETER :: NSPEC = 68 
! NVAR - Number of Variable species
  INTEGER, PARAMETER :: NVAR = 66 
! NVARACT - Number of Active species
  INTEGER, PARAMETER :: NVARACT = 56 
! NFIX - Number of Fixed species
  INTEGER, PARAMETER :: NFIX = 2 
! NREACT - Number of reactions
  INTEGER, PARAMETER :: NREACT = 142 
! NVARST - Starting of variables in conc. vect.
  INTEGER, PARAMETER :: NVARST = 1 
! NFIXST - Starting of fixed in conc. vect.
  INTEGER, PARAMETER :: NFIXST = 67 
! NONZERO - Number of nonzero entries in Jacobian
  INTEGER, PARAMETER :: NONZERO = 560 
! LU_NONZERO - Number of nonzero entries in LU factoriz. of Jacobian
  INTEGER, PARAMETER :: LU_NONZERO = 626 
! CNVAR - (NVAR+1) Number of elements in compressed row format
  INTEGER, PARAMETER :: CNVAR = 67 
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

  INTEGER, PARAMETER :: ind_H2SO4 = 1 
  INTEGER, PARAMETER :: ind_HCl = 2 
  INTEGER, PARAMETER :: ind_NH3 = 3 
  INTEGER, PARAMETER :: ind_HCOOH = 4 
  INTEGER, PARAMETER :: ind_RCOOH = 5 
  INTEGER, PARAMETER :: ind_ARO1 = 6 
  INTEGER, PARAMETER :: ind_ARO2 = 7 
  INTEGER, PARAMETER :: ind_ALK1 = 8 
  INTEGER, PARAMETER :: ind_OLE1 = 9 
  INTEGER, PARAMETER :: ind_API1 = 10 
  INTEGER, PARAMETER :: ind_API2 = 11 
  INTEGER, PARAMETER :: ind_LIM1 = 12 
  INTEGER, PARAMETER :: ind_LIM2 = 13 
  INTEGER, PARAMETER :: ind_SO2 = 14 
  INTEGER, PARAMETER :: ind_O1D = 15 
  INTEGER, PARAMETER :: ind_ANOL = 16 
  INTEGER, PARAMETER :: ind_H2O2 = 17 
  INTEGER, PARAMETER :: ind_PAN = 18 
  INTEGER, PARAMETER :: ind_TOL = 19 
  INTEGER, PARAMETER :: ind_N2O5 = 20 
  INTEGER, PARAMETER :: ind_XYL = 21 
  INTEGER, PARAMETER :: ind_CH4 = 22 
  INTEGER, PARAMETER :: ind_CRO = 23 
  INTEGER, PARAMETER :: ind_API = 24 
  INTEGER, PARAMETER :: ind_LIM = 25 
  INTEGER, PARAMETER :: ind_HNO4 = 26 
  INTEGER, PARAMETER :: ind_TO2 = 27 
  INTEGER, PARAMETER :: ind_C2H6 = 28 
  INTEGER, PARAMETER :: ind_XPAR = 29 
  INTEGER, PARAMETER :: ind_ETHOOH = 30 
  INTEGER, PARAMETER :: ind_HONO = 31 
  INTEGER, PARAMETER :: ind_CH3OOH = 32 
  INTEGER, PARAMETER :: ind_C2H4 = 33 
  INTEGER, PARAMETER :: ind_CH3OH = 34 
  INTEGER, PARAMETER :: ind_CRES = 35 
  INTEGER, PARAMETER :: ind_O3P = 36 
  INTEGER, PARAMETER :: ind_HNO3 = 37 
  INTEGER, PARAMETER :: ind_CO = 38 
  INTEGER, PARAMETER :: ind_ISOPN = 39 
  INTEGER, PARAMETER :: ind_PAR = 40 
  INTEGER, PARAMETER :: ind_OPEN = 41 
  INTEGER, PARAMETER :: ind_ISOPP = 42 
  INTEGER, PARAMETER :: ind_ISOPO2 = 43 
  INTEGER, PARAMETER :: ind_OLET = 44 
  INTEGER, PARAMETER :: ind_ISOP = 45 
  INTEGER, PARAMETER :: ind_HCHO = 46 
  INTEGER, PARAMETER :: ind_XO2 = 47 
  INTEGER, PARAMETER :: ind_AONE = 48 
  INTEGER, PARAMETER :: ind_OLEI = 49 
  INTEGER, PARAMETER :: ind_MGLY = 50 
  INTEGER, PARAMETER :: ind_ETHP = 51 
  INTEGER, PARAMETER :: ind_NAP = 52 
  INTEGER, PARAMETER :: ind_ALD2 = 53 
  INTEGER, PARAMETER :: ind_CH3O2 = 54 
  INTEGER, PARAMETER :: ind_ISOPRD = 55 
  INTEGER, PARAMETER :: ind_ROOH = 56 
  INTEGER, PARAMETER :: ind_ANO2 = 57 
  INTEGER, PARAMETER :: ind_RO2 = 58 
  INTEGER, PARAMETER :: ind_ONIT = 59 
  INTEGER, PARAMETER :: ind_NO3 = 60 
  INTEGER, PARAMETER :: ind_NO = 61 
  INTEGER, PARAMETER :: ind_NO2 = 62 
  INTEGER, PARAMETER :: ind_OH = 63 
  INTEGER, PARAMETER :: ind_O3 = 64 
  INTEGER, PARAMETER :: ind_HO2 = 65 
  INTEGER, PARAMETER :: ind_C2O3 = 66 

! Index declaration for fixed species in C
!   C(ind_spc)

  INTEGER, PARAMETER :: ind_H2O = 67 
  INTEGER, PARAMETER :: ind_M = 68 

! Index declaration for fixed species in FIX
!    FIX(indf_spc) = C(ind_spc) = C(NVAR+indf_spc)

  INTEGER, PARAMETER :: indf_H2O = 1 
  INTEGER, PARAMETER :: indf_M = 2 

END MODULE cbmz_mosaic_Parameters

