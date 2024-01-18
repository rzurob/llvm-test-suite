!#######################################################################
!*
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : F2008/generic/func/generic_allocatable_pointer_f001.f
!*  TYPE                       : Functional test
!*  FEATURE                    : #917301 F2008: Generic resolution extensions
!*  RTC Master Story           : 17301: F2008: Generic resolution extensions (master story)
!*                               https://compjazz.torolab.ibm.com:9443/jazz/resource/itemName/com.ibm.team.workitem.WorkItem/17301
!*
!*  PROGRAMMER                 : Grigor Nikolov
!*  DATE                       : 29 June 2012
!*  ORIGIN                     : XLF Test -  IBM Toronto Lab
!*
!*  DRIVER STANZA              : xlf2008 
!*  REQUIRED COMPILER OPTIONS  : 
!*  DEPENDENCIES               :
!*
!*  DESCRIPTION                : 
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

program generic_allocatable_pointer_f001
implicit none
   interface alloc_ptr_sub
      subroutine i1_alloc_sub(x,y)
         integer*1,allocatable ::x
         integer*1 y
      end  subroutine 
      subroutine i1_ptr_sub(x,y)
         integer*1, pointer ::x
         integer*1 y
      end  subroutine
      subroutine i2_alloc_sub(x,y)
         integer*2,allocatable ::x
         integer*2 y
      end  subroutine
      subroutine i2_ptr_sub(x,y)
         integer*2, pointer ::x
         integer*2 y
      end  subroutine
      subroutine i4_alloc_sub(x,y)
         integer*4,allocatable ::x
         integer*4 y
      end  subroutine
      subroutine i4_ptr_sub(x,y)
         integer*4, pointer ::x
         integer*4 y
      end  subroutine
      subroutine i8_alloc_sub(x,y)
         integer*8,allocatable ::x
         integer*8 y
      end  subroutine
      subroutine i8_ptr_sub(x,y)
         integer*8, pointer ::x
         integer*8 y
      end  subroutine

      subroutine r4_ptr_sub(x,y)
         real*4, pointer ::x
         real*4 y
      end  subroutine
      subroutine r4_alloc_sub(x,y)
         real*4,allocatable ::x
         real*4 y
      end  subroutine
      subroutine r8_ptr_sub(x,y)
         real*8, pointer ::x
         real*8 y
      end  subroutine
      subroutine r8_alloc_sub(x,y)
         real*8,allocatable ::x
         real*8 y
      end  subroutine
      subroutine r16_ptr_sub(x,y)
         real*16, pointer ::x
         real*16 y
      end  subroutine
      subroutine r16_alloc_sub(x,y)
         real*16,allocatable ::x
         real*16 y
      end  subroutine

      subroutine cx8_ptr_sub(x,y)
         complex*8, pointer ::x
         complex*8 y
      end  subroutine
      subroutine cx8_alloc_sub(x,y)
         complex*8,allocatable ::x
         complex*8 y
      end  subroutine
      subroutine cx16_ptr_sub(x,y)
         complex*16, pointer ::x
         complex*16 y
      end  subroutine
      subroutine cx16_alloc_sub(x,y)
         complex*16,allocatable ::x
         complex*16 y
      end  subroutine
      subroutine cx32_ptr_sub(x,y)
         complex*32, pointer ::x
         complex*32 y
      end  subroutine
      subroutine cx32_alloc_sub(x,y)
         complex*32,allocatable ::x
         complex*32 y
      end  subroutine

      subroutine l1_alloc_sub(x,y)
         logical*1,allocatable ::x
         logical*1 y
      end  subroutine
      subroutine l1_ptr_sub(x,y)
         logical*1, pointer ::x
         logical*1 y
      end  subroutine
      subroutine l2_alloc_sub(x,y)
         logical*2,allocatable ::x
         logical*2 y
      end  subroutine
      subroutine l2_ptr_sub(x,y)
         logical*2, pointer ::x
         logical*2 y
      end  subroutine
      subroutine l4_alloc_sub(x,y)
         logical*4,allocatable ::x
         logical*4 y
      end  subroutine
      subroutine l4_ptr_sub(x,y)
         logical*4, pointer ::x
         logical*4 y
      end  subroutine
      subroutine l8_alloc_sub(x,y)
         logical*8,allocatable ::x
         logical*8 y
      end  subroutine
      subroutine l8_ptr_sub(x,y)
         logical*8, pointer ::x
         logical*8 y
      end  subroutine

      subroutine ch8_ptr_sub(x,y)
         character(8, KIND=1), pointer ::x
         character(8, KIND=1) y
      end  subroutine
      subroutine ch8_alloc_sub(x,y)
         character(8, KIND=1),allocatable ::x
         character(8, KIND=1) y
      end  subroutine

   end interface

   interface alloc_ptr_func
      integer*1 function i1_alloc_func(x)
         integer*1,allocatable ::x
      end  function
      integer*1 function i1_ptr_func(x)
         integer*1, pointer::x
      end  function
      integer*2 function i2_alloc_func(x)
         integer*2,allocatable ::x
      end  function
      integer*2 function i2_ptr_func(x)
         integer*2, pointer::x
      end  function
      integer*4 function i4_alloc_func(x)
         integer*4,allocatable ::x
      end  function
      integer*4 function i4_ptr_func(x)
         integer*4, pointer::x
      end  function
      integer*8 function i8_alloc_func(x)
         integer*8,allocatable ::x
      end  function
      integer*8 function i8_ptr_func(x)
         integer*8, pointer::x
      end  function

      real*4 function r4_ptr_func(x)
         real*4, pointer::x
      end  function
      real*4 function r4_alloc_func(x)
         real*4,allocatable ::x
      end  function
      real*8 function r8_ptr_func(x)
         real*8, pointer::x
      end  function
      real*8 function r8_alloc_func(x)
         real*8,allocatable ::x
      end  function
      real*16 function r16_ptr_func(x)
         real*16, pointer::x
      end  function
      real*16 function r16_alloc_func(x)
         real*16,allocatable ::x
      end  function

      complex*8 function cx8_ptr_func(x)
         complex*8, pointer::x
      end  function
      complex*8 function cx8_alloc_func(x)
         complex*8,allocatable ::x
      end  function
      complex*16 function cx16_ptr_func(x)
         complex*16, pointer::x
      end  function
      complex*16 function cx16_alloc_func(x)
         complex*16,allocatable ::x
      end  function
      complex*32 function cx32_ptr_func(x)
         complex*32, pointer::x
      end  function
      complex*32 function cx32_alloc_func(x)
         complex*32,allocatable ::x
      end  function

      logical*1 function l1_alloc_func(x)
         logical*1,allocatable ::x
      end  function
      logical*1 function l1_ptr_func(x)
         logical*1, pointer::x
      end  function
      logical*2 function l2_alloc_func(x)
         logical*2,allocatable ::x
      end  function
      logical*2 function l2_ptr_func(x)
         logical*2, pointer::x
      end  function
      logical*4 function l4_alloc_func(x)
         logical*4,allocatable ::x
      end  function
      logical*4 function l4_ptr_func(x)
         logical*4, pointer::x
      end  function
      logical*8 function l8_alloc_func(x)
         logical*8,allocatable ::x
      end  function
      logical*8 function l8_ptr_func(x)
         logical*8, pointer::x
      end  function

      character(8, KIND=1) function ch8_ptr_func(x)
         character(8, KIND=1), pointer::x
      end  function
      character(8, KIND=1) function ch8_alloc_func(x)
         character(8, KIND=1),allocatable ::x
      end  function

   end interface

     integer*1               :: i1_res
     integer*1, target       :: i1_tgt /44/
     integer*1, allocatable  :: i1_alloc
     integer*1, pointer      :: i1_ptr
     integer*2               :: i2_res
     integer*2, target       :: i2_tgt /444/
     integer*2, allocatable  :: i2_alloc
     integer*2, pointer      :: i2_ptr
     integer*4               :: i4_res
     integer*4, target       :: i4_tgt /44444/
     integer*4, allocatable  :: i4_alloc
     integer*4, pointer      :: i4_ptr
     integer*8               :: i8_res
     integer*8, target       :: i8_tgt /44444444/
     integer*8, allocatable  :: i8_alloc
     integer*8, pointer      :: i8_ptr

     real*4               :: r4_res
     real*4, target       :: r4_tgt /65536.0/
     real*4, allocatable  :: r4_alloc
     real*4, pointer      :: r4_ptr
     real*8               :: r8_res
     real*8, target       :: r8_tgt /1048576.0/
     real*8, allocatable  :: r8_alloc
     real*8, pointer      :: r8_ptr
     real*16               :: r16_res
     real*16, target       :: r16_tgt /262144.0/
     real*16, allocatable  :: r16_alloc
     real*16, pointer      :: r16_ptr

     complex*8                :: cx8_res
     complex*8, target        :: cx8_tgt /(-128, 32.0)/
     complex*8, allocatable   :: cx8_alloc
     complex*8, pointer       :: cx8_ptr
     complex*16               :: cx16_res
     complex*16, target       :: cx16_tgt /(2D10,-1.0_8)/
     complex*16, allocatable  :: cx16_alloc
     complex*16, pointer      :: cx16_ptr
     complex*32               :: cx32_res
     complex*32, target       :: cx32_tgt /(-8.0_16,+8.0_16)/
     complex*32, allocatable  :: cx32_alloc
     complex*32, pointer      :: cx32_ptr

     logical*1               :: l1_res
     logical*1, target       :: l1_tgt /.FALSE._1/
     logical*1, allocatable  :: l1_alloc
     logical*1, pointer      :: l1_ptr
     logical*2               :: l2_res
     logical*2, target       :: l2_tgt /.TRUE._2/
     logical*2, allocatable  :: l2_alloc
     logical*2, pointer      :: l2_ptr
     logical*4               :: l4_res
     logical*4, target       :: l4_tgt /.FALSE._4/
     logical*4, allocatable  :: l4_alloc
     logical*4, pointer      :: l4_ptr
     logical*8               :: l8_res
     logical*8, target       :: l8_tgt /.TRUE._8/
     logical*8, allocatable  :: l8_alloc
     logical*8, pointer      :: l8_ptr

     character(8, KIND=1)               :: ch8_res
     character(8, KIND=1), target       :: ch8_tgt /'abcdefgh'/
     character(8, KIND=1), allocatable  :: ch8_alloc
     character(8, KIND=1), pointer      :: ch8_ptr

!------------------  Type: integer*1
     allocate(i1_alloc)
     i1_alloc = 77
     i1_ptr => i1_tgt

     call alloc_ptr_sub(i1_alloc, i1_res)
     if (i1_res /= 77) error stop 1 
     call alloc_ptr_sub(i1_ptr, i1_res)
     if (i1_res /= 44) error stop 2
     i1_res = alloc_ptr_func(i1_alloc)
     if (i1_res /= 77) error stop 3
     i1_res = alloc_ptr_func(i1_ptr)
     if (i1_res /= 44) error stop 4

!------------------  Type: integer*2
     allocate(i2_alloc)
     i2_alloc = 777
     i2_ptr => i2_tgt

     call alloc_ptr_sub(i2_alloc, i2_res)
     if (i2_res /= 777) error stop 1
     call alloc_ptr_sub(i2_ptr, i2_res)
     if (i2_res /= 444) error stop 2
     i2_res = alloc_ptr_func(i2_alloc)
     if (i2_res /= 777) error stop 3
     i2_res = alloc_ptr_func(i2_ptr)
     if (i2_res /= 444) error stop 4

!------------------  Type: integer*4
     allocate(i4_alloc)
     i4_alloc = 77777
     i4_ptr => i4_tgt

     call alloc_ptr_sub(i4_alloc, i4_res)
     if (i4_res /= 77777) error stop 1
     call alloc_ptr_sub(i4_ptr, i4_res)
     if (i4_res /= 44444) error stop 2
     i4_res = alloc_ptr_func(i4_alloc)
     if (i4_res /= 77777) error stop 3
     i4_res = alloc_ptr_func(i4_ptr)
     if (i4_res /= 44444) error stop 4

!------------------  Type: integer*8
     allocate(i8_alloc)
     i8_alloc = 77777777
     i8_ptr => i8_tgt

     call alloc_ptr_sub(i8_alloc, i8_res)
     if (i8_res /= 77777777) error stop 1
     call alloc_ptr_sub(i8_ptr, i8_res)
     if (i8_res /= 44444444) error stop 2
     i8_res = alloc_ptr_func(i8_alloc)
     if (i8_res /= 77777777) error stop 3
     i8_res = alloc_ptr_func(i8_ptr)
     if (i8_res /= 44444444) error stop 4

!------------------  Type: real*4
     allocate(r4_alloc)
     r4_alloc = -4.0
     r4_ptr => r4_tgt

     call alloc_ptr_sub(r4_alloc, r4_res)
     if (r4_res /= -4.0) error stop 1
     call alloc_ptr_sub(r4_ptr, r4_res)
     if (r4_res /= 65536.0) error stop 2
     r4_res = alloc_ptr_func(r4_alloc)
     if (r4_res /= -4.0) error stop 3
     r4_res = alloc_ptr_func(r4_ptr)
     if (r4_res /= 65536.0) error stop 4

!------------------  Type: real*8
     allocate(r8_alloc)
     r8_alloc = -262144.0
     r8_ptr => r8_tgt

     call alloc_ptr_sub(r8_alloc, r8_res)
     if (r8_res /= -262144.0) error stop 1
     call alloc_ptr_sub(r8_ptr, r8_res)
     if (r8_res /= 1048576.0) error stop 2
     r8_res = alloc_ptr_func(r8_alloc)
     if (r8_res /= -262144.0) error stop 3
     r8_res = alloc_ptr_func(r8_ptr)
     if (r8_res /= 1048576.0) error stop 4

!------------------  Type: real*16
     allocate(r16_alloc)
     r16_alloc = -512.0
     r16_ptr => r16_tgt

     call alloc_ptr_sub(r16_alloc, r16_res)
     if (r16_res /= -512.0) error stop 1
     call alloc_ptr_sub(r16_ptr, r16_res)
     if (r16_res /= 262144.0) error stop 2
     r16_res = alloc_ptr_func(r16_alloc)
     if (r16_res /= -512.0) error stop 3
     r16_res = alloc_ptr_func(r16_ptr)
     if (r16_res /= 262144.0) error stop 4

!------------------  Type: complex*8
     allocate(cx8_alloc)
     cx8_alloc = (4.0,-4.0)
     cx8_ptr => cx8_tgt

     call alloc_ptr_sub(cx8_alloc, cx8_res)
     if (cx8_res /= (4.0,-4.0)) error stop 1
     call alloc_ptr_sub(cx8_ptr, cx8_res)
     if (cx8_res /= (-128, 32.0)) error stop 2
     cx8_res = alloc_ptr_func(cx8_alloc)
     if (cx8_res /= (4.0,-4.0)) error stop 3
     cx8_res = alloc_ptr_func(cx8_ptr)
     if (cx8_res /= (-128, 32.0)) error stop 4

!------------------  Type: complex*16
     allocate(cx16_alloc)
     cx16_alloc = -(-8.0_16,+8.0_16)
     cx16_ptr => cx16_tgt

     call alloc_ptr_sub(cx16_alloc, cx16_res)
     if (cx16_res /= -(-8.0_16,+8.0_16)) error stop 1
     call alloc_ptr_sub(cx16_ptr, cx16_res)
     if (cx16_res /= (2D10,-1.0_8)) error stop 2
     cx16_res = alloc_ptr_func(cx16_alloc)
     if (cx16_res /= -(-8.0_16,+8.0_16)) error stop 3
     cx16_res = alloc_ptr_func(cx16_ptr)
     if (cx16_res /= (2D10,-1.0_8)) error stop 4

!------------------  Type: complex*32
     allocate(cx32_alloc)
     cx32_alloc = (-512.0,512.0)
     cx32_ptr => cx32_tgt

     call alloc_ptr_sub(cx32_alloc, cx32_res)
     if (cx32_res /= (-512.0,512.0)) error stop 1
     call alloc_ptr_sub(cx32_ptr, cx32_res)
     if (cx32_res /= (-8.0_16,+8.0_16)) error stop 2
     cx32_res = alloc_ptr_func(cx32_alloc)
     if (cx32_res /= (-512.0,512.0)) error stop 3
     cx32_res = alloc_ptr_func(cx32_ptr)
     if (cx32_res /= (-8.0_16,+8.0_16)) error stop 4


!------------------  Type: logical*1
     allocate(l1_alloc)
     l1_alloc = .TRUE._1
     l1_ptr => l1_tgt

     call alloc_ptr_sub(l1_alloc, l1_res)
     if (l1_res .NEQV. .TRUE._1) error stop 1
     call alloc_ptr_sub(l1_ptr, l1_res)
     if (l1_res .NEQV. .FALSE._1) error stop 2
     l1_res = alloc_ptr_func(l1_alloc)
     if (l1_res .NEQV. .TRUE._1) error stop 3
     l1_res = alloc_ptr_func(l1_ptr)
     if (l1_res .NEQV. .FALSE._1) error stop 4

!------------------  Type: logical*2
     allocate(l2_alloc)
     l2_alloc = .FALSE._2
     l2_ptr => l2_tgt

     call alloc_ptr_sub(l2_alloc, l2_res)
     if (l2_res .NEQV. .FALSE._2) error stop 1
     call alloc_ptr_sub(l2_ptr, l2_res)
     if (l2_res .NEQV. .TRUE._2) error stop 2
     l2_res = alloc_ptr_func(l2_alloc)
     if (l2_res .NEQV. .FALSE._2) error stop 3
     l2_res = alloc_ptr_func(l2_ptr)
     if (l2_res .NEQV. .TRUE._2) error stop 4

!------------------  Type: logical*4
     allocate(l4_alloc)
     l4_alloc = .TRUE._4
     l4_ptr => l4_tgt

     call alloc_ptr_sub(l4_alloc, l4_res)
     if (l4_res .NEQV. .TRUE._4) error stop 1
     call alloc_ptr_sub(l4_ptr, l4_res)
     if (l4_res .NEQV. .FALSE._4) error stop 2
     l4_res = alloc_ptr_func(l4_alloc)
     if (l4_res .NEQV. .TRUE._4) error stop 3
     l4_res = alloc_ptr_func(l4_ptr)
     if (l4_res .NEQV. .FALSE._4) error stop 4

!------------------  Type: logical*8
     allocate(l8_alloc)
     l8_alloc = .FALSE._8
     l8_ptr => l8_tgt

     call alloc_ptr_sub(l8_alloc, l8_res)
     if (l8_res .NEQV. .FALSE._8) error stop 1
     call alloc_ptr_sub(l8_ptr, l8_res)
     if (l8_res .NEQV. .TRUE._8) error stop 2
     l8_res = alloc_ptr_func(l8_alloc)
     if (l8_res .NEQV. .FALSE._8) error stop 3
     l8_res = alloc_ptr_func(l8_ptr)
     if (l8_res .NEQV. .TRUE._8) error stop 4

!------------------  Type: character(8, KIND=1)
     allocate(ch8_alloc)
     ch8_alloc = 'zyx'
     ch8_ptr => ch8_tgt

     call alloc_ptr_sub(ch8_alloc, ch8_res)
     if (ch8_res /= 'zyx') error stop 1
     call alloc_ptr_sub(ch8_ptr, ch8_res)
     if (ch8_res /= 'abcdefgh') error stop 2
     ch8_res = alloc_ptr_func(ch8_alloc)
     if (ch8_res /= 'zyx') error stop 3
     ch8_res = alloc_ptr_func(ch8_ptr)
     if (ch8_res /= 'abcdefgh') error stop 4

end program

      subroutine i1_alloc_sub(x,y)
         implicit none
         integer*1,allocatable ::x
         integer*1 y
         y = x
         print *, "   inside i1_alloc_sub()  x=",x,"  y=",y
      end  subroutine
      subroutine i1_ptr_sub(x,y)
         implicit none
         integer*1, pointer ::x
         integer*1 y
         y = x
         print *, "   inside i1_ptr_sub()  x=",x,"  y=",y
      end  subroutine
      integer*1 function i1_alloc_func(x)
         implicit none
         integer*1,allocatable ::x
         print *, "   inside i1_alloc_func()  x=",x
         i1_alloc_func = x
      end  function
      integer*1 function i1_ptr_func(x)
         implicit none
         integer*1, pointer::x
         print *, "   inside i1_ptr_func()  x=",x
         i1_ptr_func = x
      end  function

      subroutine i2_alloc_sub(x,y)
         implicit none
         integer*2,allocatable ::x
         integer*2 y
         y = x
         print *, "   inside i2_alloc_sub()  x=",x,"  y=",y
      end  subroutine
      subroutine i2_ptr_sub(x,y)
         implicit none
         integer*2, pointer ::x
         integer*2 y
         y = x
         print *, "   inside i2_ptr_sub()  x=",x,"  y=",y
      end  subroutine
      integer*2 function i2_alloc_func(x)
         implicit none
         integer*2,allocatable ::x
         print *, "   inside i2_alloc_func()  x=",x
         i2_alloc_func = x
      end  function
      integer*2 function i2_ptr_func(x)
         implicit none
         integer*2, pointer::x
         print *, "   inside i2_ptr_func()  x=",x
         i2_ptr_func = x
      end  function

      subroutine i4_alloc_sub(x,y)
         implicit none
         integer*4,allocatable ::x
         integer*4 y
         y = x
         print *, "   inside i4_alloc_sub()  x=",x,"  y=",y
      end  subroutine
      subroutine i4_ptr_sub(x,y)
         implicit none
         integer*4, pointer ::x
         integer*4 y
         y = x
         print *, "   inside i4_ptr_sub()  x=",x,"  y=",y
      end  subroutine
      integer*4 function i4_alloc_func(x)
         implicit none
         integer*4,allocatable ::x
         print *, "   inside i4_alloc_func()  x=",x
         i4_alloc_func = x
      end  function
      integer*4 function i4_ptr_func(x)
         implicit none
         integer*4, pointer::x
         print *, "   inside i4_ptr_func()  x=",x
         i4_ptr_func = x
      end  function

      subroutine i8_alloc_sub(x,y)
         implicit none
         integer*8,allocatable ::x
         integer*8 y
         y = x
         print *, "   inside i8_alloc_sub()  x=",x,"  y=",y
      end  subroutine
      subroutine i8_ptr_sub(x,y)
         implicit none
         integer*8, pointer ::x
         integer*8 y
         y = x
         print *, "   inside i8_ptr_sub()  x=",x,"  y=",y
      end  subroutine
      integer*8 function i8_alloc_func(x)
         implicit none
         integer*8,allocatable ::x
         print *, "   inside i8_alloc_func()  x=",x
         i8_alloc_func = x
      end  function
      integer*8 function i8_ptr_func(x)
         implicit none
         integer*8, pointer::x
         print *, "   inside i8_ptr_func()  x=",x
         i8_ptr_func = x
      end  function

      subroutine r4_alloc_sub(x,y)
         implicit none
         real*4,allocatable ::x
         real*4 y
         y = x
         print *, "   inside r4_alloc_sub()  x=",x,"  y=",y
      end  subroutine
      subroutine r4_ptr_sub(x,y)
         implicit none
         real*4, pointer ::x
         real*4 y
         y = x
         print *, "   inside r4_ptr_sub()  x=",x,"  y=",y
      end  subroutine
      real*4 function r4_alloc_func(x)
         implicit none
         real*4,allocatable ::x
         print *, "   inside r4_alloc_func()  x=",x
         r4_alloc_func = x
      end  function
      real*4 function r4_ptr_func(x)
         implicit none
         real*4, pointer::x
         print *, "   inside r4_ptr_func()  x=",x
         r4_ptr_func = x
      end  function

      subroutine r8_alloc_sub(x,y)
         implicit none
         real*8,allocatable ::x
         real*8 y
         y = x
         print *, "   inside r8_alloc_sub()  x=",x,"  y=",y
      end  subroutine
      subroutine r8_ptr_sub(x,y)
         implicit none
         real*8, pointer ::x
         real*8 y
         y = x
         print *, "   inside r8_ptr_sub()  x=",x,"  y=",y
      end  subroutine
      real*8 function r8_alloc_func(x)
         implicit none
         real*8,allocatable ::x
         print *, "   inside r8_alloc_func()  x=",x
         r8_alloc_func = x
      end  function
      real*8 function r8_ptr_func(x)
         implicit none
         real*8, pointer::x
         print *, "   inside r8_ptr_func()  x=",x
         r8_ptr_func = x
      end  function

      subroutine r16_alloc_sub(x,y)
         implicit none
         real*16,allocatable ::x
         real*16 y
         y = x
         print *, "   inside r16_alloc_sub()  x=",x,"  y=",y
      end  subroutine
      subroutine r16_ptr_sub(x,y)
         implicit none
         real*16, pointer ::x
         real*16 y
         y = x
         print *, "   inside r16_ptr_sub()  x=",x,"  y=",y
      end  subroutine
      real*16 function r16_alloc_func(x)
         implicit none
         real*16,allocatable ::x
         print *, "   inside r16_alloc_func()  x=",x
         r16_alloc_func = x
      end  function
      real*16 function r16_ptr_func(x)
         implicit none
         real*16, pointer::x
         print *, "   inside r16_ptr_func()  x=",x
         r16_ptr_func = x
      end  function

      subroutine cx8_alloc_sub(x,y)
         implicit none
         complex*8,allocatable ::x
         complex*8 y
         y = x
         print *, "   inside cx8_alloc_sub()  x=",x,"  y=",y
      end  subroutine
      subroutine cx8_ptr_sub(x,y)
         implicit none
         complex*8, pointer ::x
         complex*8 y
         y = x
         print *, "   inside cx8_ptr_sub()  x=",x,"  y=",y
      end  subroutine
      complex*8 function cx8_alloc_func(x)
         implicit none
         complex*8,allocatable ::x
         print *, "   inside cx8_alloc_func()  x=",x
         cx8_alloc_func = x
      end  function
      complex*8 function cx8_ptr_func(x)
         implicit none
         complex*8, pointer::x
         print *, "   inside cx8_ptr_func()  x=",x
         cx8_ptr_func = x
      end  function

      subroutine cx16_alloc_sub(x,y)
         implicit none
         complex*16,allocatable ::x
         complex*16 y
         y = x
         print *, "   inside cx16_alloc_sub()  x=",x,"  y=",y
      end  subroutine
      subroutine cx16_ptr_sub(x,y)
         implicit none
         complex*16, pointer ::x
         complex*16 y
         y = x
         print *, "   inside cx16_ptr_sub()  x=",x,"  y=",y
      end  subroutine
      complex*16 function cx16_alloc_func(x)
         implicit none
         complex*16,allocatable ::x
         print *, "   inside cx16_alloc_func()  x=",x
         cx16_alloc_func = x
      end  function
      complex*16 function cx16_ptr_func(x)
         implicit none
         complex*16, pointer::x
         print *, "   inside cx16_ptr_func()  x=",x
         cx16_ptr_func = x
      end  function

      subroutine cx32_alloc_sub(x,y)
         implicit none
         complex*32,allocatable ::x
         complex*32 y
         y = x
         print *, "   inside cx32_alloc_sub()  x=",x,"  y=",y
      end  subroutine
      subroutine cx32_ptr_sub(x,y)
         implicit none
         complex*32, pointer ::x
         complex*32 y
         y = x
         print *, "   inside cx32_ptr_sub()  x=",x,"  y=",y
      end  subroutine
      complex*32 function cx32_alloc_func(x)
         implicit none
         complex*32,allocatable ::x
         print *, "   inside cx32_alloc_func()  x=",x
         cx32_alloc_func = x
      end  function
      complex*32 function cx32_ptr_func(x)
         implicit none
         complex*32, pointer::x
         print *, "   inside cx32_ptr_func()  x=",x
         cx32_ptr_func = x
      end  function


      subroutine l1_alloc_sub(x,y)
         implicit none
         logical*1,allocatable ::x
         logical*1 y
         y = x
         print *, "   inside l1_alloc_sub()  x=",x,"  y=",y
      end  subroutine
      subroutine l1_ptr_sub(x,y)
         implicit none
         logical*1, pointer ::x
         logical*1 y
         y = x
         print *, "   inside l1_ptr_sub()  x=",x,"  y=",y
      end  subroutine
      logical*1 function l1_alloc_func(x)
         implicit none
         logical*1,allocatable ::x
         print *, "   inside l1_alloc_func()  x=",x
         l1_alloc_func = x
      end  function
      logical*1 function l1_ptr_func(x)
         implicit none
         logical*1, pointer::x
         print *, "   inside l1_ptr_func()  x=",x
         l1_ptr_func = x
      end  function

      subroutine l2_alloc_sub(x,y)
         implicit none
         logical*2,allocatable ::x
         logical*2 y
         y = x
         print *, "   inside l2_alloc_sub()  x=",x,"  y=",y
      end  subroutine
      subroutine l2_ptr_sub(x,y)
         implicit none
         logical*2, pointer ::x
         logical*2 y
         y = x
         print *, "   inside l2_ptr_sub()  x=",x,"  y=",y
      end  subroutine
      logical*2 function l2_alloc_func(x)
         implicit none
         logical*2,allocatable ::x
         print *, "   inside l2_alloc_func()  x=",x
         l2_alloc_func = x
      end  function
      logical*2 function l2_ptr_func(x)
         implicit none
         logical*2, pointer::x
         print *, "   inside l2_ptr_func()  x=",x
         l2_ptr_func = x
      end  function

      subroutine l4_alloc_sub(x,y)
         implicit none
         logical*4,allocatable ::x
         logical*4 y
         y = x
         print *, "   inside l4_alloc_sub()  x=",x,"  y=",y
      end  subroutine
      subroutine l4_ptr_sub(x,y)
         implicit none
         logical*4, pointer ::x
         logical*4 y
         y = x
         print *, "   inside l4_ptr_sub()  x=",x,"  y=",y
      end  subroutine
      logical*4 function l4_alloc_func(x)
         implicit none
         logical*4,allocatable ::x
         print *, "   inside l4_alloc_func()  x=",x
         l4_alloc_func = x
      end  function
      logical*4 function l4_ptr_func(x)
         implicit none
         logical*4, pointer::x
         print *, "   inside l4_ptr_func()  x=",x
         l4_ptr_func = x
      end  function

      subroutine l8_alloc_sub(x,y)
         implicit none
         logical*8,allocatable ::x
         logical*8 y
         y = x
         print *, "   inside l8_alloc_sub()  x=",x,"  y=",y
      end  subroutine
      subroutine l8_ptr_sub(x,y)
         implicit none
         logical*8, pointer ::x
         logical*8 y
         y = x
         print *, "   inside l8_ptr_sub()  x=",x,"  y=",y
      end  subroutine
      logical*8 function l8_alloc_func(x)
         implicit none
         logical*8,allocatable ::x
         print *, "   inside l8_alloc_func()  x=",x
         l8_alloc_func = x
      end  function
      logical*8 function l8_ptr_func(x)
         implicit none
         logical*8, pointer::x
         print *, "   inside l8_ptr_func()  x=",x
         l8_ptr_func = x
      end  function

      subroutine ch8_alloc_sub(x,y)
         implicit none
         character(8, KIND=1),allocatable ::x
         character(8, KIND=1) y
         y = x
         print *, "   inside ch8_alloc_sub()  x=",x,"  y=",y
      end  subroutine
      subroutine ch8_ptr_sub(x,y)
         implicit none
         character(8, KIND=1), pointer ::x
         character(8, KIND=1) y
         y = x
         print *, "   inside ch8_ptr_sub()  x=",x,"  y=",y
      end  subroutine
      character(8, KIND=1) function ch8_alloc_func(x)
         implicit none
         character(8, KIND=1),allocatable ::x
         print *, "   inside ch8_alloc_func()  x=",x
         ch8_alloc_func = x
      end  function
      character(8, KIND=1) function ch8_ptr_func(x)
         implicit none
         character(8, KIND=1), pointer::x
         print *, "   inside ch8_ptr_func()  x=",x
         ch8_ptr_func = x
      end  function

