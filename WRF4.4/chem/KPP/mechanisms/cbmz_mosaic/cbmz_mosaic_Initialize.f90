! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! Initialization File
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
! File                 : cbmz_mosaic_Initialize.f90
! Time                 : Mon Sep 26 23:36:58 2022
! Working directory    : /home/ess/xiaodong/WORK/WRFCHEM/model/WRF4.4/chem/KPP/mechanisms/cbmz_mosaic
! Equation file        : cbmz_mosaic.kpp
! Output root filename : cbmz_mosaic
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



MODULE cbmz_mosaic_Initialize

  IMPLICIT NONE

CONTAINS


! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! Initialize - function to initialize concentrations
!   Arguments :
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE Initialize ( )


  USE cbmz_mosaic_Global

  INTEGER :: i
  REAL(kind=dp) :: x

  CFACTOR = 1.000000e+00_dp

  x = (0.)*CFACTOR
  DO i = 1, NVAR
    VAR(i) = x
  END DO

  x = (0.)*CFACTOR
  DO i = 1, NFIX
    FIX(i) = x
  END DO

! constant rate coefficients
  RCONST(124) = 1
  RCONST(125) = 1
  RCONST(126) = 1
! END constant rate coefficients

! INLINED initializations

! End INLINED initializations

      
END SUBROUTINE Initialize

! End of Initialize function
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



END MODULE cbmz_mosaic_Initialize

