! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! Sparse Jacobian Data Structures File
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
! File                 : racm_soa_vbs_aqchem_JacobianSP.f90
! Time                 : Mon Sep 26 23:37:33 2022
! Working directory    : /home/ess/xiaodong/WORK/WRFCHEM/model/WRF4.4/chem/KPP/mechanisms/racm_soa_vbs_aqchem
! Equation file        : racm_soa_vbs_aqchem.kpp
! Output root filename : racm_soa_vbs_aqchem
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



MODULE racm_soa_vbs_aqchem_JacobianSP

  PUBLIC
  SAVE


! Sparse Jacobian Data


  INTEGER, PARAMETER, DIMENSION(360) :: LU_IROW_0 = (/ &
       1,  1,  1,  2,  2,  2,  3,  3,  3,  3,  3,  3, &
       3,  3,  3,  3,  3,  3,  3,  4,  4,  4,  4,  4, &
       4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4, &
       4,  4,  4,  4,  4,  4,  5,  5,  5,  6,  6,  6, &
       7,  7,  7,  8,  8,  9,  9,  9, 10, 10, 10, 11, &
      11, 11, 12, 12, 13, 13, 14, 14, 15, 15, 15, 15, &
      16, 16, 17, 17, 18, 18, 19, 19, 19, 20, 20, 21, &
      21, 21, 21, 22, 22, 23, 23, 23, 24, 24, 24, 25, &
      25, 25, 25, 25, 25, 26, 26, 26, 26, 26, 26, 27, &
      27, 27, 27, 28, 28, 28, 28, 29, 29, 29, 29, 30, &
      30, 30, 30, 30, 30, 30, 31, 31, 31, 31, 31, 31, &
      31, 32, 32, 32, 32, 32, 32, 33, 33, 33, 33, 33, &
      33, 33, 34, 34, 34, 34, 34, 34, 35, 35, 35, 35, &
      35, 35, 35, 35, 35, 35, 35, 36, 36, 36, 36, 36, &
      37, 37, 37, 37, 37, 38, 38, 38, 38, 39, 39, 39, &
      39, 39, 40, 40, 40, 40, 40, 40, 41, 41, 41, 41, &
      41, 41, 42, 42, 42, 42, 43, 43, 43, 43, 43, 43, &
      43, 43, 43, 43, 43, 44, 44, 44, 44, 44, 44, 44, &
      45, 45, 45, 45, 46, 46, 46, 46, 46, 46, 46, 46, &
      46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, &
      46, 46, 46, 46, 46, 47, 47, 47, 47, 48, 48, 48, &
      48, 49, 49, 49, 49, 50, 50, 50, 50, 50, 50, 50, &
      50, 50, 50, 51, 51, 51, 51, 52, 52, 52, 52, 52, &
      52, 52, 52, 52, 52, 52, 53, 53, 53, 53, 53, 53, &
      53, 53, 53, 53, 53, 53, 53, 54, 54, 54, 54, 54, &
      54, 55, 55, 55, 55, 55, 55, 55, 55, 55, 56, 56, &
      56, 56, 56, 56, 56, 56, 57, 57, 57, 57, 57, 57, &
      57, 57, 57, 57, 57, 57, 57, 57, 57, 58, 58, 58, &
      58, 58, 58, 58, 58, 58, 59, 59, 59, 59, 59, 59, &
      59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59 /)
  INTEGER, PARAMETER, DIMENSION(360) :: LU_IROW_1 = (/ &
      59, 59, 59, 59, 59, 59, 59, 59, 59, 60, 60, 60, &
      60, 60, 60, 60, 60, 60, 60, 61, 61, 61, 61, 61, &
      61, 61, 61, 61, 61, 61, 62, 62, 62, 62, 62, 62, &
      62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, &
      62, 62, 62, 62, 62, 62, 62, 63, 63, 63, 63, 63, &
      63, 63, 63, 63, 64, 64, 64, 64, 64, 64, 64, 64, &
      65, 65, 65, 65, 65, 65, 65, 65, 66, 66, 66, 66, &
      66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, &
      66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, &
      66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, &
      66, 66, 66, 66, 66, 66, 66, 67, 67, 67, 67, 67, &
      67, 67, 67, 67, 67, 68, 68, 68, 68, 68, 68, 68, &
      68, 68, 68, 69, 69, 69, 69, 69, 69, 69, 69, 69, &
      69, 69, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, &
      70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, &
      71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 72, &
      72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, &
      72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 73, 73, &
      73, 73, 73, 73, 73, 73, 73, 73, 74, 74, 74, 74, &
      74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, &
      74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, &
      74, 74, 74, 74, 75, 75, 75, 75, 75, 75, 75, 75, &
      75, 75, 75, 76, 76, 76, 76, 76, 76, 76, 76, 76, &
      76, 76, 76, 76, 76, 76, 76, 76, 77, 77, 77, 77, &
      77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, &
      77, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, &
      79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, &
      79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, &
      79, 79, 79, 79, 80, 80, 80, 80, 80, 80, 80, 80, &
      80, 80, 80, 80, 80, 80, 80, 81, 81, 81, 81, 81 /)
  INTEGER, PARAMETER, DIMENSION(360) :: LU_IROW_2 = (/ &
      81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, &
      81, 81, 81, 81, 81, 81, 81, 81, 81, 82, 82, 82, &
      82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, &
      82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, &
      82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, &
      82, 82, 82, 83, 83, 83, 83, 83, 83, 83, 83, 83, &
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83, 83, 83, &
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83, 83, 83, &
      83, 83, 83, 83, 84, 84, 84, 84, 84, 84, 84, 84, &
      84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, &
      84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, &
      84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, &
      84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, &
      84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, &
      84, 84, 84, 85, 85, 85, 85, 85, 85, 85, 85, 85, &
      85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, &
      85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, &
      85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, &
      85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, &
      85, 85, 85, 85, 85, 85, 85, 86, 86, 86, 86, 86, &
      86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, &
      86, 86, 86, 86, 86, 86, 86, 86, 86, 87, 87, 87, &
      87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, &
      87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, &
      87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 88, &
      88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, &
      88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, &
      88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, &
      88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, &
      88, 88, 88, 88, 88, 89, 89, 89, 89, 89, 89, 89 /)
  INTEGER, PARAMETER, DIMENSION(67) :: LU_IROW_3 = (/ &
      89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, &
      89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, &
      89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, &
      89, 89, 89, 89, 90, 90, 90, 90, 90, 90, 90, 90, &
      90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, &
      90, 90, 90, 90, 90, 90, 90 /)
  INTEGER, PARAMETER, DIMENSION(1147) :: LU_IROW = (/&
    LU_IROW_0, LU_IROW_1, LU_IROW_2, LU_IROW_3 /)

  INTEGER, PARAMETER, DIMENSION(360) :: LU_ICOL_0 = (/ &
       1, 14, 84,  2, 46, 84,  3, 22, 38, 42, 47, 48, &
      51, 54, 57, 71, 75, 79, 84,  4, 42, 47, 49, 55, &
      56, 60, 64, 65, 69, 71, 73, 75, 76, 77, 78, 79, &
      80, 81, 82, 83, 85, 90,  5,  6, 84,  6,  7, 84, &
       7,  8, 84,  8, 84,  9, 10, 84, 10, 11, 84, 11, &
      12, 84, 12, 84, 13, 79, 14, 84, 15, 60, 84, 85, &
      16, 84, 17, 84, 18, 84, 19, 88, 89, 20, 84, 21, &
      52, 84, 85, 22, 84, 23, 71, 84, 24, 41, 84, 25, &
      49, 73, 75, 79, 84, 26, 49, 73, 75, 79, 84, 27, &
      83, 84, 85, 28, 52, 84, 88, 29, 84, 85, 88, 28, &
      30, 41, 52, 84, 87, 88, 31, 36, 37, 39, 84, 87, &
      88, 20, 32, 54, 81, 83, 84, 13, 33, 51, 79, 87, &
      88, 89, 34, 50, 84, 85, 88, 89, 35, 45, 47, 48, &
      49, 51, 73, 75, 79, 84, 85, 16, 36, 79, 84, 88, &
      17, 37, 79, 84, 88, 38, 79, 84, 89, 39, 50, 79, &
      84, 88, 40, 71, 79, 82, 84, 85, 41, 48, 60, 84, &
      87, 89, 42, 79, 84, 89, 43, 50, 53, 62, 66, 71, &
      74, 84, 85, 88, 89, 44, 54, 79, 82, 84, 88, 89, &
      45, 79, 84, 89, 22, 24, 38, 41, 45, 46, 47, 48, &
      49, 51, 52, 53, 54, 57, 60, 62, 66, 71, 73, 74, &
      75, 79, 84, 87, 89, 47, 79, 84, 89, 48, 79, 84, &
      89, 49, 79, 84, 89, 34, 36, 37, 39, 50, 79, 84, &
      85, 88, 89, 51, 79, 84, 89, 21, 28, 48, 52, 57, &
      79, 84, 85, 87, 88, 89, 22, 53, 61, 67, 68, 71, &
      79, 80, 82, 83, 84, 87, 89, 54, 78, 79, 84, 88, &
      89, 38, 55, 79, 82, 83, 84, 85, 87, 89, 56, 75, &
      82, 83, 84, 85, 87, 89, 15, 42, 47, 48, 51, 57, &
      60, 63, 79, 82, 83, 84, 85, 87, 89, 45, 58, 79, &
      82, 83, 84, 85, 87, 89, 18, 23, 42, 45, 49, 56, &
      58, 59, 64, 65, 69, 70, 71, 73, 75, 76, 77, 79 /)
  INTEGER, PARAMETER, DIMENSION(360) :: LU_ICOL_1 = (/ &
      80, 81, 82, 83, 84, 85, 86, 87, 89, 48, 51, 60, &
      79, 82, 83, 84, 85, 87, 89, 39, 50, 61, 79, 82, &
      83, 84, 85, 87, 88, 89, 30, 32, 41, 48, 52, 54, &
      57, 60, 61, 62, 63, 67, 68, 71, 78, 79, 80, 81, &
      82, 83, 84, 85, 87, 88, 89, 47, 63, 79, 82, 83, &
      84, 85, 87, 89, 18, 64, 82, 83, 84, 85, 87, 89, &
      20, 65, 82, 83, 84, 85, 87, 89, 22, 24, 27, 32, &
      38, 40, 41, 42, 44, 47, 48, 49, 51, 52, 53, 54, &
      55, 56, 57, 58, 60, 61, 63, 64, 65, 66, 67, 68, &
      69, 71, 72, 73, 75, 76, 77, 78, 79, 80, 81, 82, &
      83, 84, 85, 87, 88, 89, 90, 36, 67, 79, 82, 83, &
      84, 85, 87, 88, 89, 37, 68, 79, 82, 83, 84, 85, &
      87, 88, 89, 42, 49, 69, 73, 79, 82, 83, 84, 85, &
      87, 89, 34, 50, 54, 58, 63, 64, 65, 67, 68, 70, &
      76, 77, 78, 79, 80, 82, 83, 84, 85, 87, 88, 89, &
      67, 68, 71, 79, 82, 83, 84, 85, 87, 88, 89, 16, &
      17, 40, 44, 50, 51, 54, 64, 65, 71, 72, 78, 79, &
      80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 60, 63, &
      73, 79, 82, 83, 84, 85, 87, 89, 20, 22, 23, 42, &
      45, 49, 55, 56, 58, 64, 65, 69, 70, 71, 73, 74, &
      75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, &
      87, 88, 89, 90, 47, 51, 60, 75, 79, 82, 83, 84, &
      85, 87, 89, 38, 42, 45, 47, 49, 51, 73, 75, 76, &
      77, 79, 82, 83, 84, 85, 87, 89, 38, 42, 45, 47, &
      49, 51, 73, 75, 76, 77, 79, 82, 83, 84, 85, 87, &
      89, 54, 71, 78, 79, 82, 83, 84, 85, 87, 88, 89, &
      33, 36, 37, 38, 39, 42, 45, 47, 48, 49, 50, 51, &
      54, 57, 60, 63, 71, 73, 75, 78, 79, 82, 83, 84, &
      85, 87, 88, 89, 22, 70, 76, 77, 78, 79, 80, 82, &
      83, 84, 85, 86, 87, 88, 89, 45, 47, 49, 51, 59 /)
  INTEGER, PARAMETER, DIMENSION(360) :: LU_ICOL_2 = (/ &
      64, 65, 69, 70, 71, 73, 75, 76, 77, 78, 79, 80, &
      81, 82, 83, 84, 85, 86, 87, 88, 89, 32, 40, 44, &
      48, 51, 52, 54, 55, 56, 57, 58, 59, 60, 61, 62, &
      63, 64, 65, 67, 68, 69, 70, 71, 72, 73, 74, 75, &
      76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, &
      88, 89, 90, 26, 27, 40, 48, 49, 51, 55, 56, 58, &
      60, 61, 63, 64, 65, 67, 68, 69, 71, 72, 73, 74, &
      75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, &
      87, 88, 89, 90, 13, 14, 16, 17, 18, 20, 21, 22, &
      23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 35, 36, &
      37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, &
      49, 50, 51, 52, 53, 54, 57, 59, 60, 61, 62, 63, &
      64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, &
      76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, &
      88, 89, 90, 14, 16, 17, 18, 20, 22, 23, 27, 29, &
      30, 32, 34, 35, 36, 37, 38, 39, 40, 41, 45, 46, &
      47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, &
      60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, &
      72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, &
      84, 85, 86, 87, 88, 89, 90, 55, 56, 58, 61, 63, &
      64, 65, 67, 68, 69, 72, 73, 75, 78, 79, 80, 81, &
      82, 83, 84, 85, 86, 87, 88, 89, 90, 31, 33, 36, &
      37, 39, 50, 51, 52, 55, 56, 57, 58, 60, 61, 63, &
      64, 65, 67, 68, 69, 72, 73, 75, 76, 77, 78, 79, &
      80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 19, &
      24, 28, 29, 31, 33, 34, 36, 37, 39, 41, 43, 44, &
      48, 50, 51, 52, 53, 54, 55, 56, 57, 58, 60, 61, &
      62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, &
      74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, &
      86, 87, 88, 89, 90, 19, 29, 33, 38, 42, 43, 44 /)
  INTEGER, PARAMETER, DIMENSION(67) :: LU_ICOL_3 = (/ &
      45, 47, 48, 49, 50, 51, 53, 54, 55, 56, 58, 61, &
      62, 63, 64, 65, 66, 67, 68, 69, 71, 72, 73, 74, &
      75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, &
      87, 88, 89, 90, 25, 45, 47, 49, 59, 64, 65, 69, &
      70, 71, 73, 75, 76, 77, 78, 79, 80, 81, 82, 83, &
      84, 85, 86, 87, 88, 89, 90 /)
  INTEGER, PARAMETER, DIMENSION(1147) :: LU_ICOL = (/&
    LU_ICOL_0, LU_ICOL_1, LU_ICOL_2, LU_ICOL_3 /)

  INTEGER, PARAMETER, DIMENSION(91) :: LU_CROW = (/ &
       1,  4,  7, 20, 43, 46, 49, 52, 54, 57, 60, 63, &
      65, 67, 69, 73, 75, 77, 79, 82, 84, 88, 90, 93, &
      96,102,108,112,116,120,127,134,140,147,153,164, &
     169,174,178,183,189,195,199,210,217,221,246,250, &
     254,258,268,272,283,296,302,311,319,334,343,370, &
     380,391,416,425,433,441,488,498,508,519,541,552, &
     575,585,617,628,645,662,673,701,716,742,784,821, &
     892,956,982,1020,1074,1121,1148 /)

  INTEGER, PARAMETER, DIMENSION(91) :: LU_DIAG = (/ &
       1,  4,  7, 20, 43, 46, 49, 52, 54, 57, 60, 63, &
      65, 67, 69, 73, 75, 77, 79, 82, 84, 88, 90, 93, &
      96,102,108,112,116,121,127,135,141,147,153,165, &
     170,174,178,183,189,195,199,210,217,226,246,250, &
     254,262,268,275,284,296,303,311,324,335,350,372, &
     382,400,417,426,434,466,489,499,510,528,543,562, &
     577,600,620,636,654,664,693,707,733,775,813,885, &
     950,977,1016,1071,1119,1147,1148 /)


END MODULE racm_soa_vbs_aqchem_JacobianSP
