!######################################################################
!*
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : F2008/generic/func/generic_proc_func_f004.f
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

module f2008_generic
implicit none

   integer               :: i_res /5/
contains
      function fun_a10()
      implicit none
          integer, dimension(10) :: fun_a10

          fun_a10 = 7
          print *,"       -- inside fun_a10()"
      end function

      integer function fun_int()
      implicit none

          fun_int = -8
          print *,"       -- inside fun_int()"
      end function

      subroutine sub_int(x)
      implicit none
         integer :: x

         x = x*3
          print *,"       -- inside sub_int()"
      end subroutine

!***********************************************************

      subroutine proc_fsub(x)
      implicit none
         procedure(integer) x
         integer :: y

         y = x()
         print *, "   inside proc_fsub()    y=",y
      end  subroutine

      subroutine func_fsub(x)
      implicit none
         abstract interface
            function myinterface()
              integer, dimension(10) :: myinterface
            end function
         end interface
         procedure(myinterface) :: x
         integer :: y(10)

         y = x() * 7
         print *, "   inside func_fsub()  y=",y
      end  subroutine

      subroutine proc_ssub(x)
      implicit none
         abstract interface
            subroutine myinterface(y)
              integer :: y
            end subroutine
         end interface
         procedure(myinterface) :: x
         integer :: y

         y = -100
         call x(y)
         print *, "   inside proc_ssub()    y=",y
      end  subroutine

!***********************************************************

      integer function proc_ffunc(x)
      implicit none
         procedure(integer) :: x

         proc_ffunc = 6 + x()
         print *, "   inside proc_ffunc()"
      end  function

      integer function func_ffunc(x)
      implicit none
         abstract interface
            function myinterface()
              integer, dimension(10) :: myinterface
            end function
         end interface
         procedure(myinterface) :: x
         integer :: y(10)

         y = x()
         func_ffunc = sum(y)
         print *, "   inside func_ffunc()  y=",y
      end  function

      integer function proc_sfunc(x)
      implicit none
         abstract interface
            subroutine myinterface(y)
              integer :: y
            end subroutine
         end interface
         procedure(myinterface) :: x
         integer :: y /55/

         call x(y)
         proc_sfunc = y + 7
         print *, "   inside proc_sfunc()"
      end  function

end module


program generic_proc_func_f004
implicit none
    integer inmain /1000/

    call subintr_l1(inmain)
    print *, "   inside main()  inmain=",inmain
    if (inmain /= 606) error stop 66

contains

      subroutine subintr_l1(x)
      implicit none
         integer x

         x = x + 10
         print *, "   inside subintr_l1()  x=",x
         call subintr_l2(x)
      end subroutine

      subroutine subintr_l2(x)
      implicit none
         integer x

         x = x / 5
         print *, "   inside subintr_l2()  x=",x
         call subintr_l3(x)
      end subroutine

      subroutine subintr_l3(x)
      use f2008_generic
      implicit none

         interface procfunc_sub
            module procedure proc_fsub
            module procedure func_fsub
            module procedure proc_ssub
         end interface

         interface procfunc_func
            module procedure func_ffunc
            module procedure proc_ffunc
            module procedure proc_sfunc
         end interface

        integer ::  x

         x = x * 3
         print *, "   inside subintr_l3()  x=",x

        call procfunc_sub(fun_a10)
        call procfunc_sub(fun_int)
        call procfunc_sub(sub_int)

        i_res = procfunc_func(fun_a10)
        if (i_res /= 70) error stop 3
        i_res = procfunc_func(fun_int)
        if (i_res /= -2) error stop 4
        i_res = procfunc_func(sub_int)
        if (i_res /= 172) error stop 5

      end subroutine
end program

