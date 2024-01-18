! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg020a.f
!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg020a.f
! %VERIFY: fArg020a.out:fArg020a.vf
! %STDIN:
! %STDOUT: fArg020a.out
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 05/31/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (array element ordering of
!*                              the dummy-arg)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    contains

    subroutine abc (x)
        class (*), intent(in), target :: x(2:,:)

        type seq1(k1,k2)    ! (4,8)
            integer, kind :: k1,k2
            sequence

            integer(k1)   :: i1, i2
            integer(k2)   :: i3
        end type


        type (seq1(4,8)), pointer :: s1 (:,:)
        type (seq1(4,8)), pointer :: s2 (:)
        type (seq1(4,8)), pointer :: s3

        s2 => x(:,1)
        s1 => x

        do i = 2, 4
            do j = 1, 2
                s3 => x(i, j)

                if ((s1(i,j)%i1 /= i-1) .or. (s1(i,j)%i2 /= j) .or. &
                    (s1(i,j)%i3 /= i+j-1)) error stop 1_4


                if ((s1(i,j)%i1 /= s3%i1) .or. (s1(i,j)%i2 /= s3%i2) .or. &
                    (s1(i,j)%i3 /= s3%i3)) error stop 2_4
            end do
        end do


        if (any (s2%i2 /= 1)) error stop 3_4
        do i = 1, 3
            if ((s2(i)%i1 /= i) .or. (s2(i)%i3 /= i+1)) error stop 3_4
        end do
    end subroutine

    subroutine printSeq1 (x)
        class (*), target, intent(in) :: x (:,2:)

        type seq1(k3,k4)    ! (4,8)
            integer, kind :: k3,k4
            sequence

            integer(k3)   :: i1, i2
            integer(k4)   :: i3
        end type

        type (seq1(4,8)), pointer :: s1 (:,:)
        type (seq1(4,8)), pointer :: s2 (:)
        type (seq1(4,8)), pointer :: s3

        s1 => x
        s2 => x(:, 2)

        do i = 1, ubound(x,1)
            do j = 2, ubound(x,2)
                print *, s1(i, j)
            end do
        end do

        do i = 1, ubound(s2,1)
            print *, s2(i)
        end do
    end subroutine
end module

program fArg020a
use m
    type seq1(k5,k6)    ! (4,8)
        integer, kind :: k5,k6
        sequence

        integer(k5)   :: i1, i2
        integer(k6)   :: i3
    end type

    type (seq1(4,8)) :: s1 (3, 2)

    s1 = reshape ((/((seq1(4,8) (i,j,i+j), i=1,3), j=1,2)/), (/3,2/))

    call abc (s1)

    call printSeq1 (s1(::2, 2:2))
end
