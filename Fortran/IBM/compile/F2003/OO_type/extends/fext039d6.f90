! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/13/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : EXTENDS (C438, last part: even for allocatable
!                               component, its declared type must be of a
!                               derived type that is previously defined)
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

program fext039d6
    type base
        class (base), allocatable :: data(:,:)      !<-- illegal in F2003
    end type

    type base2
        class (later), allocatable :: data(:,:)     !<-- illegal
    end type

    type later
        integer i
    end type
end
