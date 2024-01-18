! GB DTP extension using:
! ftcx_dtp -qck -qnol -qnodefaultpv -qdeferredlp -qreuse=base /tstdev/F2003/generic/assignment/functional/genericAssignmentArray016.f
! opt variations: -qnock -ql -qdefaultpv -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: with polymorphic derived type component that has generic assignment
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

   type inner(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i= 0
      contains
         generic, private :: assignment(=) => innerassgn
         procedure, pass, private :: innerassgn
   end type

   type, extends(inner) :: innerchild    ! (4)
      integer(k1) :: j = 0
   end type

   type base(k2,n1,k3)    ! (1,3,4)
      integer, kind                  :: k2,k3
      integer, len                   :: n1
      character(kind=k2,len=n1)      :: c ='xxx'
      class(inner(k3)), allocatable   :: in
      contains
         generic :: assignment(=) => bassgn1d
         procedure, pass :: bassgn1d
   end type

   contains

      subroutine bassgn1d ( a, b )
         class(base(1,*,4)), intent(inout) :: a
         type(base(1,*,4)), intent(in) :: b(:)

         a%c = b(1)%c
         if ( .not. allocated(a%in) ) allocate ( a%in, source = b(1)%in )

         a%in = b(1)%in

         print *, 'bassgn1d'

      end subroutine

      subroutine innerassgn ( a, b )
         class(inner(4)), intent(inout) :: a
         class(inner(4)), intent(in) :: b

         a%i = b%i

         select type ( a )
            type is ( innerchild(4) )
               select type ( b )
                  type is ( innerchild(4) )
                     a%j = b%j
               end select
         end select

         print *, 'innerassgn'

      end subroutine

end module

program genericAssignmentArray016
   use m

   type(base(1,3,4)) :: b1
   type(base(1,3,4)) :: b2(3)
   type(base(1,:,4)), allocatable :: b3(:)

   b2 = (/ base(1,3,4) ('aaa', inner(4)(100) ), &
           base(1,3,4) ('bbb', inner(4)(200) ), &
           base(1,3,4) ('ccc', inner(4)(300) ) /)

   allocate ( b3(4), source = (/ base(1,3,4) ('AAA', inner(4)(1000) ), &
                                 base(1,3,4) ('BBB', inner(4)(2000) ), &
                                 base(1,3,4) ('CCC', inner(4)(3000) ), &
                                 base(1,3,4) ('DDD', inner(4)(4000) ) /)  )

   print *, 'start'

   b1 = b2
   print *, b1%c, b1%in%i

   b1 = b3
   print *, b1%c, b1%in%i

   print *, 'end'

   deallocate ( b3 )

   b2 = (/ base(1,3,4) ('ddd', innerchild(4)(10, 100) ), &
           base(1,3,4) ('eee', innerchild(4)(20, 200) ), &
           base(1,3,4) ('fff', innerchild(4)(30, 300) ) /)

   print *, 'start'

   allocate ( b3(4), source = (/ base(1,3,4) ('EEE', innerchild(4)(100,1000) ), &
                                 base(1,3,4) ('FFF', innerchild(4)(200,2000) ), &
                                 base(1,3,4) ('GGG', innerchild(4)(300,3000) ), &
                                 base(1,3,4) ('HHH', innerchild(4)(400,4000) ) /)  )

   deallocate ( b1%in )

   b1 = b2
   print *, b1%c
   select type ( g => b1%in )
      type is (innerchild(4))
         print *, g%i, g%j
   end select

   b1 = b3
   print *, b1%c
   select type ( g => b1%in )
      type is (innerchild(4))
         print *, g%i, g%j
   end select

   print *, 'end'

end program
