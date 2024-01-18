! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/generic/operator/functional/genericOperatorAssociateName002.f
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
!*  DESCRIPTION                : Operator: use associate name and see if
!*                                         operator of generic tb can be used in select type
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
      integer(k1)   :: i
      contains
         procedure :: add
         generic :: operator ( + ) => add
   end type

   type, extends(base) :: child    ! (20,4)
      integer(k1) :: j
      contains
         procedure :: add => cadd
   end type

   contains

      class(base(:,4)) function add (a,b)
         class(base(*,4)), intent(in) :: a, b
         allocatable :: add

         allocate ( base(20,4) :: add )
         print *, 'add'
         add%i = a%i + b%i
      end function

      class(base(:,4)) function cadd (a,b)
         class(child(*,4)), intent(in) :: a
         class(base(*,4)), intent(in)  :: b
         allocatable :: cadd
         print *, 'cadd'
         allocate ( child(20,4) :: cadd )

         select type ( cadd )
            type is ( child(*,4) )
               select type ( b )
                  type is ( child(*,4) )
                     cadd%j = a%j + b%j
                     cadd%base = a%base + b%base
               end select
         end select


      end function

end module

program genericOperatorAssociateName002
   use m

   class(base(:,4)), pointer :: b1, b2, b3

   allocate ( b1, source = base(20,4) ( 10 ) )
   allocate ( b2, source = child(20,4) ( 10, 20 ) )

   select type ( a => b1 )
      class default
         allocate ( b3, source = a + b2 )
   end select

   print *, b3%i

   select type ( b => b2 )
      class default
         associate ( g => b + b2 )
            print *, g%i
            select type ( g )
               type is ( child(*,4) )
                  print *, g%j
            end select
         end associate
   end select
   
   select type ( b => b2 )
      class is ( base(*,4) )
         associate ( g => b + b2 )
            print *, g%i
            select type ( g )
               type is ( child(*,4) )
                  print *, g%j
            end select
         end associate
   end select 
   
end program
