! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : James Ren
!*  DATE                       : 06/07/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : function return with DTP
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

type base(baseKind)
   integer, kind :: baseKind
   integer(baseKind) :: baseId
end type

type(base(4)) :: base1, base2
base1%baseId = 10_4

base2 = funn(base1)

if (base2%baseid .ne. 10_4) stop 1

contains 
function funn(arg)
   type(base(4)) :: funn, arg
   funn = arg
end function
end

