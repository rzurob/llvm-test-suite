! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP:   fxextnp19.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f ./pubpvt.mod
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan 02, 2004
!*  ORIGIN                     : AIX Compiler Development & Test
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*                              declare external procedure entity for
!*                              different subroutines with procedure
!*                              name as pro-interface as well as no
!*                              procedure name with private/public attribute.
!*                              Invoke entities as well as passing entities
!*                              to external procedure entities in main.
!*                              Using specific interface. Using default
!*                              integer data type.
!234567890123456789012345678901234567890123456789012345678901234567890


        module pubpvt

        implicit none

        interface
            integer function ifc_int4(arg)
              integer arg
            end function
        end interface

        abstract interface
            subroutine ifc_i4(arg1, arg2)
                integer arg1
                integer arg2
            end subroutine
            subroutine ifc_i4_entry(arg1, arg2)
                integer arg1, arg2
            end subroutine
            subroutine ifc_i4_akey(arg1, arg2, arg3)
                integer arg1, arg2, arg3
            end subroutine
            recursive subroutine ifc_i4_recu(inta, intb)
              integer inta, intb
            end subroutine
            pure subroutine ifc_i4_pure(inta, intb)
              integer, intent(in)::inta
              integer, intent(out)::intb
            end subroutine
        end interface

        logical precision_R4, precision_R6, precision_R8
        logical precision_x6, precision_x8, precision_x3

        procedure(ifc_int4)::p_int4
        procedure(integer)::pp_int4

!--------declare entities using explicit procedure name

        procedure(ifc_i4),private::p_i4
        procedure(ifc_i4_entry),private::p_i4_entry
        procedure(ifc_i4_akey),private::p_i4_akey
        procedure(ifc_i4_recu),private::p_i4_recu
        procedure(ifc_i4_pure),private::p_i4_pure

!--------doesn't specify procedure name

        procedure(),public::p1_i4
        procedure(),public::p1_i4_entry
        procedure(),public::p1_i4_recu
        procedure(),public::p1_i4_pure

        integer i, j, a1, a2, a3
        integer, parameter::m=3, n=4

        integer i4_array(3,4),i4_array_dummy(3,4) , arrayrst(3,4)
        integer i4_a(7,8), i4_a_dmy(7,8)

        contains

        subroutine check()

        integer :: i4 = 430
        integer :: i4_dummy = 0

        i4_array = 11
        i4_array_dummy = 0

        i4_a = 20
        i4_a_dmy = 0


!----test 1

        call p_i4(i4, i4_dummy)

        if(i4_dummy .ne. 440)then
          error stop 1
        endif

!----test 2

        call p_i4(p_int4(i4), i4_dummy)

        if(i4_dummy .ne. 450)then
          error stop 2
        endif

!----test 3

        call p_i4(pp_int4(i4), i4_dummy)

        if(i4_dummy .ne. 460)then
          error stop 3
        endif

!----test 4

        call p1_i4_entry(i4, i4_dummy)

        if(i4_dummy .ne. 400)then
          error stop 4
        endif


!----test 9

        i4 = 6
        i4_dummy = 0

        call p_i4_recu(i4, i4_dummy)

        if(i4_dummy .ne. 720)then
            error stop 9
        endif

!----test 10

        i4 = -4
        i4_dummy = 0

        call p_i4_recu(p_int4(i4), i4_dummy)

        if(i4_dummy .ne. 720)then
            error stop 10
        endif

!----test 11

        i4 = -14
        i4_dummy = 0

        call p_i4_recu(pp_int4(i4), i4_dummy)

        if(i4_dummy .ne. 720)then
            error stop 11
        endif


!----test 15

       do i=1,7
        do j=1,8
         call p_i4_pure(i4_a(i, j), i4_a_dmy(i,j))
        end do
       end do

       if(any(i4_a_dmy .ne. 32)) then
           error stop 15
       end if

!----test 17

       a1 = 1
       a2 = 2
       a3 = 3

       call p_i4_akey(arg3 = a3, arg1 = a1, arg2 = a2)

       if((a3 .ne. 30) .or. (a2 .ne. 20)  .or. (a1 .ne. 10))then
           error stop 17
       endif

       end subroutine

      end module

      program fxextnp19

        use pubpvt

        call check

        i4 = 430
        i4_dummy = 0

        i4_array = 11
        i4_array_dummy = 0

        i4_a = 20
        i4_a_dmy = 0


!----test 5

        call p1_i4_entry(p_int4(i4), i4_dummy)

        if(i4_dummy .ne. 410)then
          error stop 5
        endif

!----test 6

        call p1_i4_entry(pp_int4(i4), i4_dummy)

        if(i4_dummy .ne. 420)then
          error stop 6
        endif

!----test 12

        i4 = 6
        i4_dummy = 0

        call p1_i4_recu(i4, i4_dummy)

        if(i4_dummy .ne. 720)then
            error stop 12
        endif

!----test 13

        i4 = -4
        i4_dummy = 0

        call p1_i4_recu(p_int4(i4), i4_dummy)

        if(i4_dummy .ne. 720)then
            error stop 13
        endif

!----test 14

        i4 = -14
        i4_dummy = 0

        call p1_i4_recu(pp_int4(i4), i4_dummy)

        if(i4_dummy .ne. 720)then
            error stop 14
        endif

!----test 16

       do i = 1,7
        do j = 1,8
          call p1_i4_pure(i4_a(i, j), i4_a_dmy(i,j))
        end do
       end do

       if(any(i4_a_dmy .ne. 42)) then
           error stop 16
       end if

       end program fxextnp19

        integer function p_int4(arg)
              integer arg
              p_int4 = arg + 10
        end function

        integer function pp_int4(arg)
              integer arg
              pp_int4 = arg + 20
        end function

        subroutine p_i4(arg1, arg2)
            integer arg1
            integer arg2
            arg2 = arg1 + 10
            return
        entry p_i4_entry(arg1, arg2)
            arg2 = arg1 + 20
            return
        end subroutine

        subroutine p_i4_akey(arg1, arg2, arg3)
             integer arg1, arg2, arg3
             arg1 = 10
             arg2 = arg1 + 10
             arg3 = arg2 + 10
        end subroutine

        recursive subroutine p_i4_recu(int, int_res)
             if (int .eq. 0) then
                int_res = 1
             else
                call p_i4_recu (int-1, int_res)
                int_res = int * int_res
             end if
        end subroutine

        pure subroutine p_i4_pure(inta, intb)
              integer, intent(in)::inta
              integer, intent(out)::intb
              intb = inta + 12
        end subroutine

        subroutine p1_i4(arg1, arg2)
            integer arg1
            integer arg2
            arg2 = arg1 - 20
            return
        entry p1_i4_entry(arg1, arg2)
            arg2 = arg1 - 30
            return
        end subroutine

        recursive subroutine p1_i4_recu(int, int_res)
             if (int .eq. 0) then
                int_res = 1
             else
                call p1_i4_recu (int-1, int_res)
                int_res = int * int_res
             end if
        end subroutine

        pure subroutine p1_i4_pure(inta, intb)
              integer, intent(in)::inta
              integer, intent(out)::intb
              intb = inta + 22
        end subroutine
