! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/generic/syntax/C463/genericC463Assignment003d.f
! opt variations: -qnok -qnol

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : C463: more and less than exactly two dummy arguments
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

   type emptybase(k1,n1)    ! (4,20)
       integer, kind :: k1
       integer, len  :: n1
      contains
         procedure, pass :: onearg
         procedure, pass :: twoargplusoneopt
         procedure, pass :: threearg

         generic :: assignment(=) => onearg
         generic :: assignment(=) => twoargplusoneopt, threearg
   end type
   
   type emptybase1(k2,n2)    ! (4,20)
       integer, kind :: k2
       integer, len  :: n2
      contains
         procedure, nopass :: noarg
         generic :: assignment(=) => noarg
   end type

   contains

      subroutine noarg ()
         print *, 'noarg'
      end subroutine

      subroutine onearg ( a )
         class(emptybase(4,*)), intent(out) :: a
         print *, 'onearg'
      end subroutine

      subroutine twoargplusoneopt ( a, b )
         class(emptybase(4,*)), intent(out) :: a
         class(emptybase(4,*)), intent(in) :: b
         print *, '2arg'
      end subroutine

      subroutine threearg ( a, b, c )
         class(emptybase(4,*)), intent(out) :: a
         class(emptybase(4,*)), intent(in) :: b, c
         print *, '3arg'
      end subroutine

end module

program genericC463Assignment003d
end

