! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/deferred/deferred004.f
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 05/26/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Deferred Binding
!*                                  - non-pure Deferred Binding in base type
!*                                    non-pure and pure binding in descendent types
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

module n

   type, abstract :: base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i
      contains
         procedure(inf), deferred, pass :: print
   end type

   interface
      subroutine inf(dtv)
         import base
         class(base(4)), intent(inout) :: dtv
      end subroutine
   end interface

   type, extends(base) :: child(k2)    ! (4,4)
      integer, kind :: k2
      integer(k2)   :: j
      contains
         procedure, pass :: print => printchild
   end type

   type, extends(child) :: gen3(k3)    ! (4,4,4)
      integer, kind :: k3
      integer(k3)   :: k
      contains
         procedure, pass :: print => cannotprintgen3
   end type

   interface
      pure subroutine cannotprintgen3(dtv)
         import gen3
         class(gen3(4,4,4)), intent(inout) :: dtv
      end subroutine
   end interface

   contains

      subroutine printchild(dtv)
         class(child(4,4)), intent(inout) :: dtv
         print *, dtv%i, dtv%j
      end subroutine

end module

pure subroutine cannotprintgen3(dtv)
   use n, only: gen3
   class(gen3(4,4,4)), intent(inout) :: dtv
   dtv%i = 123
   dtv%j = 234
   dtv%k = 345
end subroutine

program deferred004
   use n

   class(base(4)), pointer :: b1
   class(child(4,4)), allocatable, target :: c1

   allocate ( b1, source = child(4,4) ( 101, 102 ) )
   allocate ( c1, source = child(4,4) ( 103, 104 ) )

   call b1%print()
   call c1%print()

   deallocate ( b1, c1 )

   allocate ( c1, source = gen3(4,4,4)( 201, 202, 203 ) )
   
   b1 => c1
   
   select type ( b1 )
      type is ( gen3(4,4,4) )
         select type ( c1 ) 
            type is ( gen3(4,4,4) )
               call b1%child%print()
               call c1%child%print()
               if ( ( b1%k /= 203 ) .or. ( c1%k /= 203 ) ) error stop 1_4
         end select
   end select
  
   call b1%print()

   select type ( b1 )
      type is ( gen3(4,4,4) )
         select type ( c1 ) 
            type is ( gen3(4,4,4) )
               call b1%child%print()
               call c1%child%print()
               if ( ( b1%k /= 345 ) .or. ( c1%k /= 345 ) ) error stop 1_4
         end select
   end select

end program
