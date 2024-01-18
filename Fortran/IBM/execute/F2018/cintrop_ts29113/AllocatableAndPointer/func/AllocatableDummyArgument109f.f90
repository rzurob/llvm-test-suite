! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : AllocatableDummyArgument106f.f
!*
!* PROGRAMMER                   : Dorra Bouchiha
!* DATE                         : January 25, 2013
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: ALLOCATABLE and POINTER dummy argument
!* SECONDARY FUNTIONS TESTED    :
!*
!* DRIVER STANZA                :
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Calling a Fortran BIND(C) procedure from Fortran
!*
!*                                - Allocatable / Pointer Scalar various interoperable types
!*                                - Generic resolution based 
!*                                  * on type 
!*                                  * attribute (allocatable vs. pointer)
!*                                - List-directed I/O
!*
!234567890123456789012345678901234567890123456789012345678901234567890
program AllocatableDummyArgument109f
use iso_c_binding
implicit none

   interface alloc_ptr_sub
      subroutine i1_alloc_sub(x,y) bind(c)
         use iso_c_binding, only: c_signed_char
         implicit none
         integer(c_signed_char),allocatable ::x
         integer(c_signed_char) y
      end  subroutine 
      subroutine i1_ptr_sub(x,y) bind(c)
         use iso_c_binding, only: c_signed_char
         implicit none
         integer(c_signed_char), pointer ::x
         integer(c_signed_char) y
      end  subroutine
      subroutine i2_alloc_sub(x,y) bind(c)
         use iso_c_binding, only: c_short
         implicit none
         integer(c_short),allocatable ::x
         integer(c_short) y
      end  subroutine
      subroutine i2_ptr_sub(x,y) bind(c)
         use iso_c_binding, only: c_short
         implicit none
         integer(c_short), pointer ::x
         integer(c_short) y
      end  subroutine
      subroutine i4_alloc_sub(x,y) bind(c)
         use iso_c_binding, only: c_int
         implicit none
         integer(c_int),allocatable ::x
         integer(c_int) y
      end  subroutine
      subroutine i4_ptr_sub(x,y) bind(c)
         use iso_c_binding, only: c_int
         implicit none
         integer(c_int), pointer ::x
         integer(c_int) y
      end  subroutine
      subroutine i8_alloc_sub(x,y) bind(c)
         use iso_c_binding, only: c_long_long
         implicit none
         integer(c_long_long),allocatable ::x
         integer(c_long_long) y
      end  subroutine
      subroutine i8_ptr_sub(x,y) bind(c)
         use iso_c_binding, only: c_long_long
         implicit none
         integer(c_long_long), pointer ::x
         integer(c_long_long) y
      end  subroutine

      subroutine r4_ptr_sub(x,y) bind(c)
         use iso_c_binding, only: c_float
         implicit none
         real(c_float), pointer ::x
         real(c_float) y
      end  subroutine
      subroutine r4_alloc_sub(x,y) bind(c)
         use iso_c_binding, only: c_float
         implicit none
         real(c_float),allocatable ::x
         real(c_float) y
      end  subroutine
      subroutine r8_ptr_sub(x,y) bind(c)
         use iso_c_binding, only: c_double
         implicit none
         real(c_double), pointer ::x
         real(c_double) y
      end  subroutine
      subroutine r8_alloc_sub(x,y) bind(c)
         use iso_c_binding, only: c_double
         implicit none
         real(c_double),allocatable ::x
         real(c_double) y
      end  subroutine
      subroutine r16_ptr_sub(x,y) bind(c)
         use iso_c_binding, only: c_long_double
         implicit none
         real(c_long_double), pointer ::x
         real(c_long_double) y
      end  subroutine
      subroutine r16_alloc_sub(x,y) bind(c)
         use iso_c_binding, only: c_long_double
         implicit none
         real(c_long_double),allocatable ::x
         real(c_long_double) y
      end  subroutine

      subroutine cx8_ptr_sub(x,y) bind(c)
         use iso_c_binding, only: c_float_complex
         implicit none
         complex(c_float_complex), pointer ::x
         complex(c_float_complex) y
      end  subroutine 
      subroutine cx8_alloc_sub(x,y) bind(c)
         use iso_c_binding, only: c_float_complex
         implicit none
         complex(c_float_complex),allocatable ::x
         complex(c_float_complex) y
      end  subroutine
      subroutine cx16_ptr_sub(x,y) bind(c)
         use iso_c_binding, only: c_double_complex
         implicit none
         complex(c_double_complex), pointer ::x
         complex(c_double_complex) y
      end  subroutine
      subroutine cx16_alloc_sub(x,y) bind(c)
         use iso_c_binding, only: c_double_complex
         implicit none
         complex(c_double_complex),allocatable ::x
         complex(c_double_complex) y
      end  subroutine
      subroutine cx32_ptr_sub(x,y) bind(c)
         use iso_c_binding, only: c_long_double_complex
         implicit none
         complex(c_long_double_complex), pointer ::x
         complex(c_long_double_complex) y
      end  subroutine
      subroutine cx32_alloc_sub(x,y) bind(c)
         use iso_c_binding, only: c_long_double_complex
         implicit none
         complex(c_long_double_complex),allocatable ::x
         complex(c_long_double_complex) y
      end  subroutine

      subroutine l_alloc_sub(x,y) bind(c)
         use iso_c_binding, only: c_bool
         implicit none
         logical(c_bool),allocatable ::x
         logical(c_bool) y
      end  subroutine
      subroutine l_ptr_sub(x,y) bind(c)
         use iso_c_binding, only: c_bool
         implicit none
         logical(c_bool), pointer ::x
         logical(c_bool) y
      end  subroutine

      subroutine char_ptr_sub(x) bind(c)
         use iso_c_binding, only: c_char
         implicit none
         character(:,kind=c_char),pointer ::x
      end  subroutine
      subroutine char_alloc_sub(x) bind(c)
         use iso_c_binding, only: c_char
         implicit none
         character(:,kind=c_char),allocatable ::x
      end  subroutine
   end interface

     integer(c_signed_char)  :: i1_res
     integer(c_signed_char), target       :: i1_tgt /44/
     integer(c_signed_char), allocatable  :: i1_alloc
     integer(c_signed_char), pointer      :: i1_ptr
     integer(c_short)               :: i2_res
     integer(c_short), target       :: i2_tgt /444/
     integer(c_short), allocatable  :: i2_alloc
     integer(c_short), pointer      :: i2_ptr
     integer(c_int)               :: i4_res
     integer(c_int), target       :: i4_tgt /44444/
     integer(c_int), allocatable  :: i4_alloc
     integer(c_int), pointer      :: i4_ptr
     integer(c_long_long)               :: i8_res
     integer(c_long_long), target       :: i8_tgt /44444444/
     integer(c_long_long), allocatable  :: i8_alloc
     integer(c_long_long), pointer      :: i8_ptr

     real(c_float)               :: r4_res
     real(c_float), target       :: r4_tgt /65536.0/
     real(c_float), allocatable  :: r4_alloc
     real(c_float), pointer      :: r4_ptr
     real(c_double)               :: r8_res
     real(c_double), target       :: r8_tgt /1048576.0/
     real(c_double), allocatable  :: r8_alloc
     real(c_double), pointer      :: r8_ptr
     real(c_long_double)               :: r16_res
     real(c_long_double), target       :: r16_tgt /262144.0/
     real(c_long_double), allocatable  :: r16_alloc
     real(c_long_double), pointer      :: r16_ptr

     complex(c_float_complex)                :: cx8_res
     complex(c_float_complex), target        :: cx8_tgt /(-128, 32.0)/
     complex(c_float_complex), allocatable   :: cx8_alloc
     complex(c_float_complex), pointer       :: cx8_ptr
     complex(c_double_complex)               :: cx16_res
     complex(c_double_complex), target       :: cx16_tgt /(2D10,-1.0_8)/
     complex(c_double_complex), allocatable  :: cx16_alloc
     complex(c_double_complex), pointer      :: cx16_ptr
     complex(c_long_double_complex)               :: cx32_res
     complex(c_long_double_complex), target       :: cx32_tgt /(-8.0_16,+8.0_16)/
     complex(c_long_double_complex), allocatable  :: cx32_alloc
     complex(c_long_double_complex), pointer      :: cx32_ptr

     logical(c_bool)         :: l1_res
     logical(c_bool), target       :: l1_tgt /.FALSE._1/
     logical(c_bool), allocatable  :: l1_alloc
     logical(c_bool), pointer      :: l1_ptr

     character(8, KIND=c_char)               :: ch8_res
     character(8, KIND=c_char), target       :: ch8_tgt /'abcdefgh'/
     character(:, KIND=c_char), allocatable  :: ch8_alloc
     character(:, KIND=c_char), pointer      :: ch8_ptr

!------------------  Type: integer(c_signed_char)
     allocate(i1_alloc)
     i1_alloc = 77
     i1_ptr => i1_tgt

     call alloc_ptr_sub(i1_alloc, i1_res)
     if (i1_res /= 77) error stop 1 
     call alloc_ptr_sub(i1_ptr, i1_res)
     if (i1_res /= 44) error stop 2

!------------------  Type: integer(c_short)
     allocate(i2_alloc)
     i2_alloc = 777
     i2_ptr => i2_tgt

     call alloc_ptr_sub(i2_alloc, i2_res)
     if (i2_res /= 777) error stop 3
     call alloc_ptr_sub(i2_ptr, i2_res)
     if (i2_res /= 444) error stop 4

!------------------  Type: integer(c_int)
     allocate(i4_alloc)
     i4_alloc = 77777
     i4_ptr => i4_tgt

     call alloc_ptr_sub(i4_alloc, i4_res)
     if (i4_res /= 77777) error stop 5
     call alloc_ptr_sub(i4_ptr, i4_res)
     if (i4_res /= 44444) error stop 6

!------------------  Type: integer(c_long_long)
     allocate(i8_alloc)
     i8_alloc = 77777777
     i8_ptr => i8_tgt

     call alloc_ptr_sub(i8_alloc, i8_res)
     if (i8_res /= 77777777) error stop 7
     call alloc_ptr_sub(i8_ptr, i8_res)
     if (i8_res /= 44444444) error stop 8

!------------------  Type: real(c_float)
     allocate(r4_alloc)
     r4_alloc = -4.0
     r4_ptr => r4_tgt

     call alloc_ptr_sub(r4_alloc, r4_res)
     if (r4_res /= -4.0) error stop 9
     call alloc_ptr_sub(r4_ptr, r4_res)
     if (r4_res /= 65536.0) error stop 10

!------------------  Type: real(c_double)
     allocate(r8_alloc)
     r8_alloc = -262144.0
     r8_ptr => r8_tgt

     call alloc_ptr_sub(r8_alloc, r8_res)
     if (r8_res /= -262144.0) error stop 11
     call alloc_ptr_sub(r8_ptr, r8_res)
     if (r8_res /= 1048576.0) error stop 12

!------------------  Type: real(c_long_double)
     allocate(r16_alloc)
     r16_alloc = -512.0
     r16_ptr => r16_tgt

     call alloc_ptr_sub(r16_alloc, r16_res)
     if (r16_res /= -512.0) error stop 13
     call alloc_ptr_sub(r16_ptr, r16_res)
     if (r16_res /= 262144.0) error stop 14

!------------------  Type: complex(c_float_complex)
     allocate(cx8_alloc)
     cx8_alloc = (4.0,-4.0)
     cx8_ptr => cx8_tgt

     call alloc_ptr_sub(cx8_alloc, cx8_res)
     if (cx8_res /= (4.0,-4.0)) error stop 15
     call alloc_ptr_sub(cx8_ptr, cx8_res)
     if (cx8_res /= (-128, 32.0)) error stop 16

!------------------  Type: complex(c_double_complex)
     allocate(cx16_alloc)
     cx16_alloc = -(-8.0_16,+8.0_16)
     cx16_ptr => cx16_tgt

     call alloc_ptr_sub(cx16_alloc, cx16_res)
     if (cx16_res /= -(-8.0_16,+8.0_16)) error stop 17
     call alloc_ptr_sub(cx16_ptr, cx16_res)
     if (cx16_res /= (2D10,-1.0_8)) error stop 18

!------------------  Type: complex(c_long_double_complex)
     allocate(cx32_alloc)
     cx32_alloc = (-512.0,512.0)
     cx32_ptr => cx32_tgt

     call alloc_ptr_sub(cx32_alloc, cx32_res)
     if (cx32_res /= (-512.0,512.0)) error stop 19
     call alloc_ptr_sub(cx32_ptr, cx32_res)
     if (cx32_res /= (-8.0_16,+8.0_16)) error stop 20


!------------------  Type: logical(c_bool)
     allocate(l1_alloc)
     l1_alloc = .TRUE._1
     l1_ptr => l1_tgt

     call alloc_ptr_sub(l1_alloc, l1_res)
     if (l1_res .NEQV. .TRUE._1) error stop 21
     call alloc_ptr_sub(l1_ptr, l1_res)
     if (l1_res .NEQV. .FALSE._1) error stop 22

!------------------  Type: character(8, KIND=c_char)
     allocate(ch8_alloc, source=ch8_tgt)
     ch8_alloc = 'zyx'
     ch8_ptr => ch8_tgt

     call alloc_ptr_sub(ch8_alloc)
     call alloc_ptr_sub(ch8_ptr)

end program AllocatableDummyArgument109f

      subroutine i1_alloc_sub(x,y) bind(c)
         use iso_c_binding, only: c_signed_char
         implicit none
         integer(c_signed_char),allocatable ::x
         integer(c_signed_char) y
         y = x
         print *, "   inside i1_alloc_sub()  x=",x,"  y=",y
      end  subroutine
      subroutine i1_ptr_sub(x,y) bind(c)
         use iso_c_binding, only: c_signed_char
         implicit none
         integer(c_signed_char), pointer ::x
         integer(c_signed_char) y
         y = x
         print *, "   inside i1_ptr_sub()  x=",x,"  y=",y
      end  subroutine

      subroutine i2_alloc_sub(x,y) bind(c)
         use iso_c_binding, only: c_short
         implicit none
         integer(c_short),allocatable ::x
         integer(c_short) y
         y = x
         print *, "   inside i2_alloc_sub()  x=",x,"  y=",y
      end  subroutine
      subroutine i2_ptr_sub(x,y) bind(c)
         use iso_c_binding, only: c_short
         implicit none
         integer(c_short), pointer ::x
         integer(c_short) y
         y = x
         print *, "   inside i2_ptr_sub()  x=",x,"  y=",y
      end  subroutine

      subroutine i4_alloc_sub(x,y) bind(c)
         use iso_c_binding, only: c_int
         implicit none
         integer(c_int),allocatable ::x
         integer(c_int) y
         y = x
         print *, "   inside i4_alloc_sub()  x=",x,"  y=",y
      end  subroutine
      subroutine i4_ptr_sub(x,y) bind(c)
         use iso_c_binding, only: c_int
         implicit none
         integer(c_int), pointer ::x
         integer(c_int) y
         y = x
         print *, "   inside i4_ptr_sub()  x=",x,"  y=",y
      end  subroutine

      subroutine i8_alloc_sub(x,y) bind(c)
         use iso_c_binding, only: c_long_long
         implicit none
         integer(c_long_long),allocatable ::x
         integer(c_long_long) y
         y = x
         print *, "   inside i8_alloc_sub()  x=",x,"  y=",y
      end  subroutine
      subroutine i8_ptr_sub(x,y) bind(c)
         use iso_c_binding, only: c_long_long
         implicit none
         integer(c_long_long), pointer ::x
         integer(c_long_long) y
         y = x
         print *, "   inside i8_ptr_sub()  x=",x,"  y=",y
      end  subroutine

      subroutine r4_alloc_sub(x,y) bind(c)
         use iso_c_binding, only: c_float
         implicit none
         real(c_float),allocatable ::x
         real(c_float) y
         y = x
         print *, "   inside r4_alloc_sub()  x=",x,"  y=",y
      end  subroutine
      subroutine r4_ptr_sub(x,y) bind(c)
         use iso_c_binding, only: c_float
         implicit none
         real(c_float), pointer ::x
         real(c_float) y
         y = x
         print *, "   inside r4_ptr_sub()  x=",x,"  y=",y
      end  subroutine

      subroutine r8_alloc_sub(x,y) bind(c)
         use iso_c_binding, only: c_double
         implicit none
         real(c_double),allocatable ::x
         real(c_double) y
         y = x
         print *, "   inside r8_alloc_sub()  x=",x,"  y=",y
      end  subroutine
      subroutine r8_ptr_sub(x,y) bind(c)
         use iso_c_binding, only: c_double
         implicit none
         real(c_double), pointer ::x
         real(c_double) y
         y = x
         print *, "   inside r8_ptr_sub()  x=",x,"  y=",y
      end  subroutine

      subroutine r16_alloc_sub(x,y) bind(c)
         use iso_c_binding, only: c_long_double
         implicit none
         real(c_long_double),allocatable ::x
         real(c_long_double) y
         y = x
         print *, "   inside r16_alloc_sub()  x=",x,"  y=",y
      end  subroutine
      subroutine r16_ptr_sub(x,y) bind(c)
         use iso_c_binding, only: c_long_double
         implicit none
         real(c_long_double), pointer ::x
         real(c_long_double) y
         y = x
         print *, "   inside r16_ptr_sub()  x=",x,"  y=",y
      end  subroutine

      subroutine cx8_alloc_sub(x,y) bind(c)
         use iso_c_binding, only: c_float_complex
         implicit none
         complex(c_float_complex),allocatable ::x
         complex(c_float_complex) y
         y = x
         print *, "   inside cx8_alloc_sub()  x=",x,"  y=",y
      end  subroutine
      subroutine cx8_ptr_sub(x,y) bind(c)
         use iso_c_binding, only: c_float_complex
         implicit none
         complex(c_float_complex), pointer ::x
         complex(c_float_complex) y
         y = x
         print *, "   inside cx8_ptr_sub()  x=",x,"  y=",y
      end  subroutine

      subroutine cx16_alloc_sub(x,y) bind(c)
         use iso_c_binding, only: c_double_complex
         implicit none
         complex(c_double_complex),allocatable ::x
         complex(c_double_complex) y
         y = x
         print *, "   inside cx16_alloc_sub()  x=",x,"  y=",y
      end  subroutine
      subroutine cx16_ptr_sub(x,y) bind(c)
         use iso_c_binding, only: c_double_complex
         implicit none
         complex(c_double_complex), pointer ::x
         complex(c_double_complex) y
         y = x
         print *, "   inside cx16_ptr_sub()  x=",x,"  y=",y
      end  subroutine

      subroutine cx32_alloc_sub(x,y) bind(c)
         use iso_c_binding, only: c_long_double_complex
         implicit none
         complex(c_long_double_complex),allocatable ::x
         complex(c_long_double_complex) y
         y = x
         print *, "   inside cx32_alloc_sub()  x=",x,"  y=",y
      end  subroutine
      subroutine cx32_ptr_sub(x,y) bind(c)
         use iso_c_binding, only: c_long_double_complex
         implicit none
         complex(c_long_double_complex), pointer ::x
         complex(c_long_double_complex) y
         y = x
         print *, "   inside cx32_ptr_sub()  x=",x,"  y=",y
      end  subroutine

      subroutine l_alloc_sub(x,y) bind(c)
         use iso_c_binding, only: c_bool
         implicit none
         logical(c_bool),allocatable ::x
         logical(c_bool) y
         y = x
         print *, "   inside l_alloc_sub()  x=",x,"  y=",y
      end  subroutine
      subroutine l_ptr_sub(x,y) bind(c)
         use iso_c_binding, only: c_bool
         implicit none
         logical(c_bool), pointer ::x
         logical(c_bool) y
         y = x
         print *, "   inside l_ptr_sub()  x=",x,"  y=",y
      end  subroutine

      subroutine char_alloc_sub(x) bind(c)
         use iso_c_binding, only: c_char
         implicit none
         character(:,kind=c_char),allocatable ::x
         !character(*,kind=c_char) y // not supported yet
         print *, "   inside char_alloc_sub()  x=",x
      end  subroutine
      subroutine char_ptr_sub(x) bind(c)
         use iso_c_binding, only: c_char
         implicit none
         character(:,kind=c_char), pointer ::x
         print *, "   inside char_ptr_sub()  x=",x
      end  subroutine

