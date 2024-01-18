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
    type base(k)
        integer, kind :: k
        class (base(k)), allocatable :: data(:,:)      !<-- illegal in F2003
    end type

    type base2(k)
        integer, kind :: k
        class (later(k)), allocatable :: data(:,:)     !<-- illegal
    end type

    type later(k)
        integer, kind :: k
        integer(k) :: i
    end type
end
