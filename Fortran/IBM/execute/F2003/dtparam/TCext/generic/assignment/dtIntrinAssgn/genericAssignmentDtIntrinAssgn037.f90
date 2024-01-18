! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qnodeferredlp -qreuse=self -qreuse=base /tstdev/F2003/generic/assignment/dtIntrinAssgn/genericAssignmentDtIntrinAssgn037.f
! opt variations: -qck -qnol -qdefaultpv -qdeferredlp -qreuse=none

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
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                 - more components (scalar and array) and several have generic assignment defined
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

module m


   type com1(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)      i
      contains
         procedure :: c1a
         generic :: assignment(=) => c1a
   end type

   type com2(n2,k2)    ! (20,4)
      integer, kind :: k2
      integer, len  :: n2
      integer(k2)      i
      contains
         procedure :: c2a
         generic :: assignment(=) => c2a
   end type

   type, extends(com2) :: c_com2    ! (20,4)
      integer(k2) :: j
      contains
         procedure :: c2a => cc2a
   end type

   type com3(n3,k3)    ! (20,4)
      integer, kind :: k3
      integer, len  :: n3
      sequence
      integer(k3)      i
   end type

   contains

      elemental subroutine c1a ( a, b )
         class(com1(*,4)) , intent(out) :: a
         class(com1(*,4)) , intent(in)  :: b

         a%i = b%i + 1

      end subroutine

      elemental subroutine c2a ( a, b )
         class(com2(*,4)) , intent(out) :: a
         class(com2(*,4)) , intent(in)  :: b

         a%i = b%i + 1

      end subroutine

      elemental subroutine cc2a ( a, b )
         class(c_com2(*,4)) , intent(out) :: a
         class(com2(*,4)) , intent(in)  :: b

         a%i = b%i + 1

         select type ( b )
            type is ( c_com2(*,4) )
               a%j = b%j + 1
         end select

      end subroutine

end module

module n
   use m, only: com1, com2, c_com2, com3

   type container(n4,k4,n5)    ! (3,4,20)
      integer, kind                   :: k4
      integer, len                    :: n4,n5
      character(n4)                   :: c1
      type(com1(n5,k4))               :: c11(3)
      integer(k4)                     :: i(2)
      class(com1(n5,k4)), allocatable :: c12(:)
      type(com2(n5,k4)), allocatable  :: c21(:)
      class(com2(n5,k4)), allocatable :: c22(:)
      type(c_com2(n5,k4))             :: cc21(2)
      type(com3(n5,k4))               :: c31(2)
      type(com3(n5,k4)), allocatable  :: c32(:)
      contains
         procedure :: writeformatted
         generic :: write(formatted) => writeformatted
   end type

   contains

      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         class(container(*,4,*)), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg

         write ( unit, * , iostat = iostat, iomsg = iomsg ) dtv%c1, dtv%c11%i, dtv%i, dtv%c12%i, dtv%c21%i
         select type ( g => dtv%c22 )
            type is ( com2(*,4) )
               write ( unit, * , iostat = iostat, iomsg = iomsg ) g%i
            type is ( c_com2(*,4) )
               write ( unit, * , iostat = iostat, iomsg = iomsg ) g%i, g%j
         end select

         write ( unit, * , iostat = iostat, iomsg = iomsg ) dtv%cc21%i, dtv%cc21%j, dtv%c31%i, dtv%c32%i

      end subroutine

end module

program genericAssignmentDtIntrinAssgn037
  use n

   type(container(3,4,20)) :: c1, c2, c3
   allocatable :: c2
   pointer :: c3

   allocate ( c2, c3 )

   c1 = container(3,4,20) ( 'abc', (/ com1(20,4)(1), com1(20,4)(2), com1(20,4)(3) /), (/ 4,5 /), (/ com1(20,4)(6), com1(20,4)(7), com1(20,4)(8) /), (/ com2(20,4)(9), com2(20,4)(10) /), (/ com2(20,4)(11), com2(20,4)(12), com2(20,4)(13) /), (/ c_com2(20,4)(14,15), c_com2(20,4)(16,17) /), (/ com3(20,4)(18), com3(20,4)(19) /), (/ com3(20,4)(20) /) )
   print *, c1

   c2 = c1
   print *, c2

   c3 = c2
   print *, c3

   c2 = container(3,4,20) ( 'ABC', (/ com1(20,4)(11), com1(20,4)(12), com1(20,4)(13) /), (/ 14,15 /),(/ com1(20,4)(16), com1(20,4)(17), com1(20,4)(18) /) , (/ com2(20,4)(19), com2(20,4)(20) /), (/ c_com2(20,4)(21,22), c_com2(20,4)(23,24), c_com2(20,4)(25,26) /), (/ c_com2(20,4)(27,28), c_com2(20,4)(29,30) /), (/ com3(20,4)(31), com3(20,4)(32) /), (/ com3(20,4)(33) /) )
   print *, c2

   c1 = c2
   print *, c1

   c3 = c1
   print *, c3

end program
