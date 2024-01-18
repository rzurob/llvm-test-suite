!*******************************************************************************
!*
!============================================================================
!USE ONLY
!*
!============================================================================
!*
!*  TEST CASE NAME             : do_concurrent_f028.f
!*
!*  DATE                       : 2015-08-31
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : DO CONCURRENT (F2008 extension)
!*  SECONDARY FUNCTIONS TESTED :
!*  ADAPTED FROM               :
!*
!*  DESCRIPTION                :
!*    - acceptance test for do concurrent with different ways of declaring the
!*      type in integer_type_spec
!*    - DO CONCURRENT keyword should be case insensitive
!*
!=============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890
       program main
        integer, parameter :: i1 = 1
        integer, parameter :: i2 = 2
        integer, parameter :: i4 = 4
        integer, parameter :: i8 = 8
        integer, parameter :: lbound = 1
        integer, parameter :: ubound = 20
        integer, parameter :: step = 1
        character*32 :: arg = 'one'

        ! Type specified in brackets.  Type with * is already extensively tested
        do concurrent (integer(1) :: i = 1:20)
        end do

        DO concurrent (integer(2) :: i = 1:20)
        end do

        do CONCURRENT (integer(4) :: i = 1:20)
        end do

        DO CONCURRENT (integer(8) :: i = 1:20)
        end do

        dO cOnCuRrEnT (integer(16) :: i = 1:20)
        end do

        do concurrent (integer(i1) :: i = 1:20)
        end do

        do concurrent (integer(i2) :: i = 1:20)
        end do

        do concurrent (integer(i4) :: i = 1:20)
        end do

        do concurrent (integer(i8) :: i = 1:20)
        end do

        do concurrent (integer(i2) :: i = lbound:20:step)
        end do

        do concurrent (integer(i2) :: i = lbound:20)
        end do

        do concurrent (integer(i4) :: i = 1:ubound:step)
        end do

        do concurrent (integer(i8) :: i = lbound:ubound)
        end do

        ! verification of go-to, error stop, and stop
        call getarg(1, arg)
        if (arg .eq. 'ten') then
          do concurrent (i = 1:1:1)
            go to 1010
              do concurrent(j=1:1)
              end do
1010        error stop  10
          end do
        end if

        print *, "arg=", arg
        ! nested case of above
        if (arg .eq. 'twenty') then
          do concurrent (i = 1:1:1)
            do concurrent (j = 1:1:1)
              go to 1020
1020          stop 20
            end do
          end do
        end if

       end
