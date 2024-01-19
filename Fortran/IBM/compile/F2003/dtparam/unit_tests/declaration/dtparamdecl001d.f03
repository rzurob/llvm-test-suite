! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/12/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : type parameters, type parameter name can
!*                               not conflict with component/binding
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
program dtparamdecl001d

  type baseproc
     integer  :: basekind
     contains
       !  expect a message indicating identifier basekind is already a binding/component/type parameter
       !   of the same derived type
       procedure basekind
  end type

  type baseproc2
     integer  :: basekind
     !  expect a message indicating identifier basekind is already a component/type parameter
     !   of the same derived type
     integer  :: basekind
  end type

  type base(basekind, baselen)
     integer, kind :: basekind
     integer, len :: baselen
     contains
       !  expect a message indicating identifier basekind is already a binding/component/type parameter
       !   of the same derived type
       procedure basekind
  end type

  type base2(basekind, baselen)
     integer, kind :: basekind
     integer, len :: baselen
     !  expect a message indicating identifier basekind is already a component/type parameter
     !   of the same derived type
     integer  :: basekind
  end type

  type base3(basekind, basekind)
     integer, kind :: basekind
  end type

end

