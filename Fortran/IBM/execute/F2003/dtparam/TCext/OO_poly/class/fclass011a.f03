! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all /tstdev/OO_poly/class/fclass011a.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/21/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : CLASS (unlimited poly allocatable arrays
!                               structure components in intrinsic assignment)
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
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class (*), allocatable :: data (:)
    end type

    contains

    subroutine printBase (b)
        class(base(4,*)), intent(in) :: b(:)

        do i = 1, size (b)
            print *, 'element ', i

            if (allocated (b(i)%data)) then
                print *, 'bounds:', lbound(b(i)%data), ubound(b(i)%data)

                select type (x => b(i)%data)
                    type is (real(4))
                        print *, 'real type with kind = ', kind(x)
                        print '(f10.2)', x
                    type is (real(8))
                        print *, 'real type with kind = ', kind(x)
                        print '(d10.2)', x
                    type is (real(16))
                        print *, 'real type with kind = ', kind(x)
                        print '(g12.3)', x
                    type is (complex(4))
                        print *, 'complex type with kind = ', kind(x)
                        write (*, 100) x
                    type is (complex(8))
                        print *, 'complex type with kind = ', kind(x)
                        write (*, 200) x
                end select
            end if
        end do
100 format ('(', f10.2, ',', f10.2,')')
200 format ('(', d10.2, ',', d10.2,')')
    end subroutine
end module

program fclass011a
use m
    type (base(4,20)) b1(5)

    class (base(4,20)), allocatable :: b2(:)

    allocate (b2(5))

    allocate (b2(1)%data(0:0), source=1.78e0_8)

    allocate (b2(2)%data(3), source= (/2.47e0_4, 1.34e0_4, -1.22e0_4/))
    allocate (b2(3)%data(0:1), source=(/(1.23e0_4, 3.21e0_4), (-1.44e0_4, &
                                        11.23e0_4)/))

    allocate (b2(4)%data(2), source=(/(1.78e0_8, 3.56e0_8), (-3.14e0_8, &
                                        1.34e0_8)/))

    allocate (b2(5)%data(1), source=3.1415927e0_16)
    !! intrinsic assignment
    b1 = b2

    call printBase(b1)
end
