! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_type/abstract/C611/dtio001.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 09/28/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: If the rightmost part-name is of abstract type, data-ref shall be polymorphic. (C611)
!*                                        R614: structure-component is data-ref
!*                                        non-polymorphic abstract type data-ref appears in IO statements ( with or without dtio )
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

   type, abstract :: base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: id
   end type

   type, extends(base) :: child    ! (20,4)
      real(k1) :: rid
   end type

end module

program dtio001
   use m, newb => base

   interface write(unformatted)
      subroutine aaa ( a, unit, iostat, iomsg  )
         import newb
         class(newb(*,4)), intent(in) :: a
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   class(newb(:,4)), allocatable :: a
   type(child(20,4)) :: c

   allocate ( a, source = child(20,4)(1,2.3))

   select type ( a )
      type is ( child(*,4) )
         associate ( gg => c )
            write ( 1 ) a%base, 555, gg%base
            print *, a%base, 555, gg%base
         end associate
   end select
   
   

end program

subroutine aaa ( a, unit, iostat, iomsg  )
   use m, only: base
   class(base(*,4)), intent(in) :: a
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   
   write ( unit ) a%id
   
end subroutine
