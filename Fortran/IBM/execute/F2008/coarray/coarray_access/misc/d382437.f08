! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-12-23
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : defect 382437
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

program d382437
    real, save :: x(3)[*]

    if (num_images() < 2) then
        print *, 'requires at least two images to run'
        error stop 1
    end if

    x = [(i, i = 1, 3)]*this_image()

    sync all

    if (this_image() == 1) then
        print *, sin(x(:)[2])
        print *, sin ([2,4,6]*1.0)
    end if
    end
