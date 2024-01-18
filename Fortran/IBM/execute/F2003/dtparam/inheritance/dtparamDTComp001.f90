!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 12/19/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam
!                               Case: Test multiple parameterized derived type
!                               components; test a collector of real values.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type realVal (k)
        integer, kind :: k = 4

        real(k) value
    end type

    type node (k)
        integer, kind :: k

        type(realVal(k)) data

        type(node(k)), pointer :: next => null()
    end type
end module

module m1
use m
    type collector
        type(node(4)), pointer :: real4List => null()
        type(node(8)), pointer :: real8List => null()
        type(node(16)), pointer :: real16List => null()
    end type

    type(collector), protected, save :: co1m

    contains

    !! this routine adds to the input data, d, to co1m's lists according to the
    !   following rules: 1.) |d| < 1.0 --> co1m%real4List; 2.) 1.0 <= |d| < 10.0
    !   --> co1m%real8List; 3.) others go to co1m%real16List
    subroutine add2Collector (d)
        real(8), intent(in) :: d

        type (node(4)), pointer :: r4Tail => null()
        type (node(8)), pointer :: r8Tail => null()
        type (node(16)), pointer :: r16Tail => null()

        if (abs(d) < 1.0d0) then    !! add to co1m%real4List
            if (.not. associated(r4Tail)) then
                allocate (co1m%real4List, source=node(4)(realVal(4)(d), &
                        null()))
                r4Tail => co1m%real4List
            else
                allocate (r4Tail%next, source=node(4)(realVal(4)(d),null()))

                r4Tail => r4Tail%next
            end if
        else if (abs(d) < 1.0d1) then !co1m%real8List
            if (.not. associated(r8Tail)) then
                allocate (co1m%real8List, source=node(8)(data=realVal(8)(d), &
                        next=null()))
                r8Tail => co1m%real8List
            else
                allocate (r8Tail%next, source=node(8)(realVal(8)(value=d), &
                        null()))

                r8Tail => r8Tail%next
            end if
        else        !co1m%real16List
            if (.not. associated(r16Tail)) then
                allocate (co1m%real16List, source=node(16)(realVal(16)(d), &
                        null()))
                r16Tail => co1m%real16List
            else
                allocate (r16Tail%next, source=node(16)(realVal(16)(d),null()))

                r16Tail => r16Tail%next
            end if
        end if
    end subroutine

    subroutine printCollector
        type(node(4)), pointer :: r4Iterator
        type(node(8)), pointer :: r8Iterator
        type(node(16)), pointer :: r16Iterator

        integer count(3)

        count = 0

        !! do the print and count
        r4Iterator => co1m%real4List
        r8Iterator => co1m%real8List
        r16Iterator => co1m%real16List

        print *, 'between -1.0 and +1.0'

        do while (associated(r4Iterator))
            write (*, '(f6.3)') r4Iterator%data

            count(1) = count(1) + 1
            r4Iterator => r4Iterator%next
        end do

        print *, 'less than +10.0, or greater than -10.0'

        do while (associated(r8Iterator))
            write (*, '(f12.5)') r8Iterator%data

            count(2) = count(2) + 1
            r8Iterator => r8Iterator%next
        end do

        print *, 'others'

        do while (associated(r16Iterator))
            write (*, '(g12.3)') r16Iterator%data

            count(3) = count(3) + 1
            r16Iterator => r16Iterator%next
        end do

        print *, 'Total: real*4 --> ', count(1), '; real*8 --> ', count(2), &
            '; real*16 --> ', count(3)
    end subroutine
end module

program dtparamDTComp001
use m1
    real(8) d1
    integer istat

1   read(*, *, iostat=istat) d1
        if (istat /= 0) goto 10

        call add2Collector (d1)
    goto 1

10  continue

    call printCollector
end
