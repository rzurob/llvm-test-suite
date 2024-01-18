! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/generic/operator/functional/genericOperatorPure002.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

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
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Binary Operator: UD operator subroutine is a pure function, with class hierarchy and
!*                                                child type defined another UD operator
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

   type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: x = -999
      contains
         procedure, pass :: mybadd
         generic :: operator(*) => mybadd
   end type

   type, extends(base) :: child    ! (20,4)
      integer(k1) :: y = -999
      contains
         procedure, pass :: mybaddarray
         generic :: operator(*) => mybaddarray
   end type

   contains

   class(base(:,4)) pure function mybadd ( a, b )
      class(base(*,4)), intent(in) :: a
      integer, intent(in) :: b
      allocatable :: mybadd

      select type ( a )
         type is ( base(*,4) )
            allocate ( base(20,4):: mybadd )
            mybadd%x = a%x * b
         type is ( child(*,4) )
            allocate ( child(20,4) :: mybadd )
            select type ( mybadd )
               type is ( child(*,4) )
                  mybadd%x = a%x * b
                  mybadd%y = a%y * b
            end select
      end select

   end function

   class(base(:,4)) pure function mybaddarray ( a, b )
      class(child(*,4)), intent(in) :: a
      integer, intent(in) :: b(:)
      allocatable :: mybaddarray

      allocate ( child(20,4) :: mybaddarray )
      select type ( mybaddarray )
         type is ( child(*,4) )
            mybaddarray%x = a%x * b(1)
            mybaddarray%y = a%y * b(1)
      end select

   end function

end module

program genericOperatorPure002
   use m

   type(base(20,4)) :: b1
   class(base(:,4)), allocatable :: b2
   class(base(:,4)), pointer :: b3

   b1 = base(20,4)(100) * 10
   allocate ( b2, source = b1 * 100  )
   allocate ( b3, source = child(20,4)(100,200) * (/ 100, 200 /) )

   print *, b1%x  
   print *, b2%x
   select type ( b3 )
      type is ( child(*,4) )
         print *, b3%x, b3%y
   end select

end program
