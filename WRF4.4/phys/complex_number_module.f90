































        module complex_number_module

        implicit none


        integer, parameter :: loc_real_precision = 8

        real (kind=loc_real_precision), parameter, private :: min_val = 1.0e-30_loc_real_precision

        type complex_number
          real(kind=loc_real_precision) :: real_part, imag_part
        end type complex_number

        interface c_add
          module procedure c_add_cc,    &      
                           c_add_cr,    &      
                           c_add_rc            
        end interface

        interface c_sub
          module procedure c_sub_cc,    &      
                           c_sub_cr,    &      
                           c_sub_rc            
        end interface

        interface c_mul
          module procedure c_mul_cc,    &      
                           c_mul_rc            
        end interface

        interface c_div
          module procedure c_div_cc,    &      
                           c_div_rc            
        end interface

        contains


        type (complex_number) function c_set (x, y)



          real(kind=loc_real_precision), intent(in) :: x, y

          character (len = 80) :: str

          write (str, *) x
          read(str, *) c_set%real_part
          write (str, *) y
          read(str, *) c_set%imag_part

        end function c_set


        type (complex_number) function c_add_cc (z1, z2)

          type (complex_number), intent(in) :: z1, z2

          c_add_cc%real_part = z1%real_part + z2%real_part
          c_add_cc%imag_part = z1%imag_part + z2%imag_part

        end function c_add_cc


        type (complex_number) function c_add_cr (z3, num1)

          type (complex_number),         intent(in) :: z3
          real(kind=loc_real_precision), intent(in) :: num1

          c_add_cr%real_part = z3%real_part + num1
          c_add_cr%imag_part = z3%imag_part

        end function c_add_cr


        type (complex_number) function c_add_rc (num2, z4)

          type (complex_number),         intent(in) :: z4
          real(kind=loc_real_precision), intent(in) :: num2

          c_add_rc%real_part = z4%real_part + num2
          c_add_rc%imag_part = z4%imag_part

        end function c_add_rc


        type (complex_number) function c_sub_cc (z1, z2)

          type (complex_number), intent(in) :: z1, z2

          c_sub_cc%real_part = z1%real_part - z2%real_part
          c_sub_cc%imag_part = z1%imag_part - z2%imag_part

        end function c_sub_cc


        type (complex_number) function c_sub_cr (z3, num1)

          type (complex_number),         intent(in) :: z3
          real(kind=loc_real_precision), intent(in) :: num1

          c_sub_cr%real_part = z3%real_part - num1
          c_sub_cr%imag_part = z3%imag_part

        end function c_sub_cr


        type (complex_number) function c_sub_rc (num2, z4)

          type (complex_number),         intent(in) :: z4
          real(kind=loc_real_precision), intent(in) :: num2

          c_sub_rc%real_part = num2 - z4%real_part
          c_sub_rc%imag_part = - z4%imag_part

        end function c_sub_rc


        type (complex_number) function c_mul_cc (z1, z2)

          type (complex_number), intent(in) :: z1, z2

          c_mul_cc%real_part =   z1%real_part * z2%real_part    &
                               - z1%imag_part * z2%imag_part
          c_mul_cc%imag_part =   z1%real_part * z2%imag_part    &
                               + z1%imag_part * z2%real_part

        end function c_mul_cc


        type (complex_number) function c_mul_rc (x, z1)

          type (complex_number),         intent(in) :: z1
          real(kind=loc_real_precision), intent(in) :: x

          c_mul_rc%real_part = z1%real_part * x
          c_mul_rc%imag_part = z1%imag_part * x

        end function c_mul_rc


        type (complex_number) function c_div_cc (z1, z2)

          type (complex_number), intent(in) :: z1, z2

          real(kind=loc_real_precision) :: denom
          real(kind=loc_real_precision) :: temp(2)

          denom = 1.0 / (  z2%real_part * z2%real_part &
                         + z2%imag_part * z2%imag_part)

          c_div_cc%real_part = (  z1%real_part * z2%real_part          &
                                + z1%imag_part * z2%imag_part) * denom

          temp(1) = abs(c_div_cc%real_part)
          temp(2) = min_val

          c_div_cc%real_part = sign(maxval(temp), c_div_cc%real_part)

          c_div_cc%imag_part = (  z1%imag_part * z2%real_part          &
                                - z1%real_part * z2%imag_part) * denom

          temp(1) = abs(c_div_cc%imag_part)

          c_div_cc%imag_part = sign(maxval(temp), c_div_cc%imag_part)

        end function c_div_cc


        type (complex_number) function c_div_rc (num, z1)



          real(kind=loc_real_precision), intent(in) :: num
          type (complex_number),         intent(in) :: z1

          real(kind=loc_real_precision) :: denom, temp

          temp = z1%real_part * z1%real_part + z1%imag_part * z1%imag_part
          temp = sign(max(abs(temp), min_val), temp)

          denom = num / temp
          c_div_rc%real_part = z1%real_part * denom
          c_div_rc%imag_part = -1.0 * z1%imag_part * denom

        end function c_div_rc


        type (complex_number) function c_sin (z1)



          type (complex_number), intent(in) :: z1

          c_sin%real_part = sin(z1%real_part) * cosh(z1%imag_part)
          c_sin%imag_part = cos(z1%real_part) * sinh(z1%imag_part)

        end function c_sin


        type (complex_number) function c_cos (z1)

          type (complex_number), intent(in) :: z1

          c_cos%real_part = cos(z1%real_part) * cosh(z1%imag_part)
          c_cos%imag_part = -1.0 * sin(z1%real_part) * sinh(z1%imag_part)

        end function c_cos


        real(kind=loc_real_precision) function c_abs (z1)



          type (complex_number), intent(in) :: z1

          c_abs = sqrt(z1%real_part**2 + z1%imag_part**2)

        end function c_abs

        end module complex_number_module
