! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/25/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items
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

program fmisc022
    type base
        class(*), pointer :: data(:)    !<-- class(*) is not of importance
    end type

    type (base) :: b1

    data b1%data /null()/

    if (associated (b1%data)) error stop 1_4
    end
