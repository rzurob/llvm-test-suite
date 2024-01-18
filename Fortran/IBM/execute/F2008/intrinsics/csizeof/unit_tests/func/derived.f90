      program t

      use, intrinsic :: iso_c_binding      
      implicit none
      integer(c_size_t) :: rt
	  
      type, bind(c) :: my_type1
      integer(C_SHORT) :: s(2)
      end type                     !  4
	
      type, bind(c) :: my_type2
      real(c_float) r1             !  4  
      real(c_long_double) r2       ! 16 
      real(c_double) r3            !  8
      end type                     ! 28
	
      type, bind(c) :: my_type3
      character(len=1) str2        !  1  
      integer(c_short) sint1(3)    !  6  sint1+str2 = 8
      complex(c_double_complex) c1 ! 16 
      integer(c_int8_t) bint       !  1  
      logical(c_bool) bool         !  1  bint+bool = 4
      end type !28
	
      type, bind(c) :: my_type4
      logical(c_bool) bool         !  1
      type(my_type3) :: t_f_34     ! 28	
      end type                     ! 32
	  
      type(my_type1)::dt1_array(20)
      type(my_type1)::dt1_scalar

      type(my_type2)::dt2_array(3)
      type(my_type2)::dt2_scalar

      type(my_type3)::dt3_array(7)
      type(my_type3)::dt3_scalar

      type(my_type4)::dt4_array(1)
      type(my_type4)::dt4_scalar
	  
      type(c_ptr) :: pd
      type(c_funptr) :: pf

      interface
        integer function get_sizeof_my_type1()
        end function

        integer function get_sizeof_my_type2()
        end function

        integer function get_sizeof_my_type3()
        end function

        integer function get_sizeof_my_type4()
        end function
      end interface

      integer sizeof_my_type1
      integer sizeof_my_type2
      integer sizeof_my_type3
      integer sizeof_my_type4

      sizeof_my_type1 = get_sizeof_my_type1()
      sizeof_my_type2 = get_sizeof_my_type2()
      sizeof_my_type3 = get_sizeof_my_type3()
      sizeof_my_type4 = get_sizeof_my_type4()
	  
      rt = c_sizeof(dt1_scalar) !4
      if (rt /= sizeof_my_type1) error stop 1

      rt = c_sizeof(dt1_array)  !4*20
      if (rt /= 20 * sizeof_my_type1) error stop 2

      rt = c_sizeof(dt1_array(2))
      if (rt /= sizeof_my_type1) error stop 3

      rt = c_sizeof(dt1_array(1:7))
      if (rt /= 7 * sizeof_my_type1) error stop 4

      rt = c_sizeof(dt2_scalar) !28
      if (rt /= sizeof_my_type2) error stop 5

      rt = c_sizeof(dt2_array) !28*3
      if (rt /= 3 * sizeof_my_type2) error stop 6

      rt = c_sizeof(dt2_array(2))
      if (rt /= sizeof_my_type2) error stop 7

      rt = c_sizeof(dt2_array(1:2))
      if (rt /= 2 * sizeof_my_type2) error stop 8

      rt = c_sizeof(dt3_scalar) !28
      if (rt /= sizeof_my_type3) error stop 9

      rt = c_sizeof(dt3_array)  !28*7
      if (rt /= 7 * sizeof_my_type3) error stop 10

      rt = c_sizeof(dt3_array(2))
      if (rt /= sizeof_my_type3) error stop 11

      rt = c_sizeof(dt3_array(1:7))
      if (rt /= 7 * sizeof_my_type3) error stop 12

      rt = c_sizeof(dt4_scalar)!32
      if (rt /= sizeof_my_type4) error stop 13

      rt = c_sizeof(dt4_array) !32*1
      if (rt /= sizeof_my_type4) error stop 14

      rt = c_sizeof(dt4_array(1))
      if (rt /= sizeof_my_type4) error stop 15

      rt = c_sizeof(dt4_array(1:1))
      if (rt /= sizeof_my_type4) error stop 16

      rt = c_sizeof(pd)
      if (rt /= c_long) error stop 17

      rt = c_sizeof(pf)
      if (rt /= c_long) error stop 18
  
      end 
