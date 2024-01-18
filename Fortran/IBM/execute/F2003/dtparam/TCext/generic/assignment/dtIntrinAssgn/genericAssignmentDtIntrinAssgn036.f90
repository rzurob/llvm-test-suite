! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/F2003/generic/assignment/dtIntrinAssgn/genericAssignmentDtIntrinAssgn036.f
! opt variations: -qck -qnol -qdefaultpv -qnodeferredlp -qreuse=self -qreuse=base

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                 - more components and several have generic assignment defined
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

   type, extends(com2) :: c_com2(n3,k3)    ! (20,4,20,4)
      integer, kind :: k3
      integer, len  :: n3
      integer(k3)   :: j
      contains
         procedure :: c2a => cc2a
   end type

   type com3(n4,k4)    ! (20,4)
      integer, kind :: k4
      integer, len  :: n4
      sequence
      integer(k4)      i
   end type

   contains

      subroutine c1a ( a, b )
         class(com1(*,4)) , intent(out) :: a
         class(com1(*,4)) , intent(in)  :: b

         a%i = b%i

         print *, 'c1a'
      end subroutine

      subroutine c2a ( a, b )
         class(com2(*,4)) , intent(out) :: a
         class(com2(*,4)) , intent(in)  :: b

         a%i = b%i

         print *, 'c2a'
      end subroutine

      subroutine cc2a ( a, b )
         class(c_com2(*,4,*,4)) , intent(out) :: a
         class(com2(*,4)) , intent(in)  :: b

         a%i = b%i

         select type ( b )
            type is ( c_com2(*,4,*,4) )
               a%j = b%j
         end select

         print *, 'cc2a'
      end subroutine

end module

module n
   use m, only: com1, com2, c_com2, com3

   type container(n5,k5,n6)    ! (3,4,20)
      integer, kind                  :: k5
      integer, len                   :: n5,n6
      character(n5)                  :: c1
      type(com1(n6,k5))              :: c11
      integer(k5)                    :: i
      class(com1(:,k5)), allocatable :: c12
      type(com2(:,k5)), allocatable   :: c21
      class(com2(:,k5)), allocatable :: c22
      type(c_com2(n6,k5,n6,k5))      :: cc21
      type(com3(n6,k5))              :: c31
      type(com3(:,k5)), allocatable  :: c32
      contains
         procedure :: writeformatted
         generic :: write(formatted) => writeformatted
         procedure, pass :: return
   end type

   contains

      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         class(container(*,4,*)), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg

         write ( unit, "(a3,4(1x,i4))" , iostat = iostat, iomsg = iomsg, advance = 'no' ) dtv%c1, dtv%c11%i, dtv%i, dtv%c12%i, dtv%c21%i
         select type ( g => dtv%c22 )
            type is ( com2(*,4) )
               write ( unit, "(1x,i4)" , iostat = iostat, iomsg = iomsg, advance = 'no' ) g%i
            type is ( c_com2(*,4,*,4) )
               write ( unit, "(1x,i4,1x,i4)" , iostat = iostat, iomsg = iomsg, advance = 'no' ) g%i, g%j
         end select

         write ( unit, "(4(1x,i4))" , iostat = iostat, iomsg = iomsg, advance = 'no' ) dtv%cc21%i, dtv%cc21%j, dtv%c31%i, dtv%c32%i

      end subroutine

      type(container(3,4,20)) function return ( a )
         class(container(*,4,*)), intent(in) :: a
         return = a
      end function

end module

program genericAssignmentDtIntrinAssgn036
  use n

   type(container(3,4,20)) :: c1, c2, c3
   allocatable :: c2
   pointer :: c3

   allocate ( c2, c3 )

   c1 = container(3,4,20) ( 'abc', com1(20,4)(1), 2, com1(20,4)(3), com2(20,4)(4), com2(20,4)(5), c_com2(20,4,20,4)(6,7), com3(20,4)(8), com3(20,4)(9) )
   print *, c1

   c2 = c1
   print *, c2

   c3 = c2
   print *, c3

   c2 = container(3,4,20) ( 'ABC', com1(20,4)(11), 12, com1(20,4)(13), com2(20,4)(14), c_com2(20,4,20,4)(15,16), c_com2(20,4,20,4)(17,18), com3(20,4)(19), com3(20,4)(20) )
   print *, c2

   c1 = c2
   print *, c1

   c3 = c1
   print *, c3

   c3 = container(3,4,20) (c11 = com1(20,4)(21), c32 = com3(20,4)(30), c22 = c_com2(20,4,20,4)(25,26) ,c31 = com3(20,4)(29) , c12 = com1(20,4)(23), i = 22, c1 = 'IBM',  cc21 = c_com2(20,4,20,4)(27,28), c21 =  com2(20,4)(24)  )
   print *, c3

   c2 = c3%return()
   print *, c2

   c1 = return(c2)
   print *, c1

end program
