! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/18/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (unlimited
!                               poly-allocatable components in structure
!                               constructor; use scalars of complex type and
!                               real type)
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

program fconstr033a
    class (*), allocatable :: x
    class (*), pointer :: x1(:)

    type base
        class (*), allocatable :: data
    end type

    real(8), target :: r1(10)

    allocate (x, source=(1.0d2, 1.4d1))

    x1 => r1

    r1 = (/(k*1.1d0, k= 1, 10)/)

    associate (y1 => base(x), y2 => base(x1(5)))
        select type (z => y1%data)
            type is (complex(8))
                write (*, '(2d12.3)') z
            type is (complex(4))
                print *,'bad'
            class default
                print *, 'wrong'
        end select

        select type (z => y2%data)
            type is (real(8))
                write (*, '(d12.3)') z
            type is (real(4))
                print *, 'bad'
            class default
                print *, 'wrong'
        end select
    end associate
end
