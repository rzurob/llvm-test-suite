! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/31/2009
!*
!*  DESCRIPTION                : miscellaneous(defect 367823)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    interface
        function foo (x, y)
            real, allocatable, intent(in) :: x(:,:), y(:,:)

            real, allocatable :: foo(:,:)
        end function
    end interface

    real, allocatable :: x1(:,:), y1(:,:)

    allocate (x1(10,10), y1(10,10))

    print *, foo(x1+1,y1)
    z = foo(x1, y-1)
    end