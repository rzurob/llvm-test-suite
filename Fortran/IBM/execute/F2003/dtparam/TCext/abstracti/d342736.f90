!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-16
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Abstract Interface
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DEFECT ABSTRACT            : DTPARAM: DTIO: No WRITE() for Extened Type
!*                               Component with Length Parameter
!*
!*  DESCRIPTION                :
!*  The Reduced Code below implements the Extended Derived Type "child"
!*  with a Length Parameter.  The DTIO WRITE() for this Type has been
!*  implemented through the SUBROUTINE "writeUnformatted()".
!*
!*  When the DTIO WRITE() is invoked, this Test Case fails to write the
!*  Extended Derived Type Component (although the corresponding SELECT TYPE
!*  Statement appears to be correctly executed).
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
   type, abstract :: base
      character :: c(3)
   end type

   type, extends(base) :: child (lchild_1) ! lchild_1=3
      integer, len :: lchild_1
      character :: cc(lchild_1)
   end type

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program d342736
   use m1

   class(base), pointer     :: b3, b4(:)
   character(200) :: msg1 = ''
   integer :: stat1
   integer(4)    :: pos1
   integer(4)    :: pos( 5 )
   integer(4)    :: extendedTypeWrite
   common /dtio/ pos1, pos, extendedTypeWrite
   character(1) :: ch1
   character(3) :: ch3

   allocate ( b3, source = child(3) ( (/'g','h','i'/), (/'G','H','I'/) ) ) ! tcx: (3)
   allocate ( b4(1), source = (/ b3 /) )

   open ( 1, file = 'pos004.data', form='unformatted', access='stream' )
   inquire ( 1, pos = pos1 )

   write ( 1, iostat = stat1, iomsg=msg1, pos = pos1 )                  b4

    rewind( 1 )

    print *, "pos( 1 ) =", pos( 1 )

    do i = 1, 3
       read( 1 ) ch1
       print *, "pos(", (i + 1), ") =", pos( (i + 1) ), "ch1(", i, ") = (", ch1, ")"
    end do

    print *, "pos( 5 ) =", pos( 5 ), ", extendedTypeWrite =", extendedTypeWrite
    read( 1 ) ch3
    print *, "ch3 = (", ch3, ")"

   if ( pos( 1 ) /= pos1     )    error stop 37
   if ( pos( 2 ) /= pos1 + 1 )    error stop 38
   if ( pos( 3 ) /= pos1 + 2 )    error stop 39
   if ( pos( 4 ) /= pos1 + 3 )    error stop 40
   if ( pos( 5 ) /= pos1 + 6 )    error stop 41

end program d342736

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base, child
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   integer(4)    :: pos1, pos2, pos3, pos4, pos5
   integer(4)    :: pos1main
   integer(4)    :: extendedTypeWrite
   common /dtio/ pos1main, pos1, pos2, pos3, pos4, pos5, extendedTypeWrite

   inquire ( unit, pos = pos1 )

   write ( unit )   dtv%c(1)
   inquire ( unit, pos = pos2 )

   write ( unit )   dtv%c(2)
   inquire ( unit, pos = pos3 )

   write ( unit )   dtv%c(3)
   inquire ( unit, pos = pos4 )

   extendedTypeWrite = 0
   select type ( dtv )
      type is (child(*)) ! tcx: (*)
         write ( unit ) dtv%cc
         extendedTypeWrite = 99
   end select

   inquire ( unit, pos = pos5 )

!  if ( pos2 /= pos1 + 1 )    error stop 38
!  if ( pos3 /= pos1 + 2 )    error stop 39
!  if ( pos4 /= pos1 + 3 )    error stop 40
!  if ( pos5 /= pos1 + 6 )    error stop 41

end subroutine
