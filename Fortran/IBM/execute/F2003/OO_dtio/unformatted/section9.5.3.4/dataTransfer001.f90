!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: dataTransfer001.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 9.5.3.4: Data Transfer Order
!*                               - All values are transmitted to or from the entites
!*                                 specified by a list item prior to processing of any
!*                                 succeeding list item for all data tranfer I/O stmt.
!*                               Sequential Access
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
   type base
      integer :: c = 0
      contains
         procedure, pass :: getC
         procedure, pass :: setC
   end type

contains
   function getC (a)
      class(base), intent(in) :: a
      integer :: getC
      getC = a%c
   end function

   subroutine setC (a, char)
      class(base), intent(inout) :: a
      integer, intent(in) :: char
      a%c = char
   end subroutine
end module


program dataTransfer001
   use m1

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables
   class(base), allocatable :: b1(:)
   class(base), pointer     :: b2(:)
   type(base),  allocatable :: b3(:,:)
   integer :: stat
   character(200) :: msg

   integer :: myunit = 1
   integer :: result(4)


   ! allocation of variables

   allocate (b1(5), source = (/ (base(i),i=1,5)  /) )
   allocate (b2(5), source = (/ (base(j),j=6,10) /) )
   allocate (b3(1:5,6:10) )

   b3(1,6)%c = 20

   open (unit = 1, file ='dataTransfer001.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   ! the following should write myunit=1, b1(myunit)=b1(1)=1, b2(b1(myunit))=b2(1)=6,b3(b1(myunit),b2(b1(myunit)))=b3(1,6)= 20

   write (myunit, iostat=stat, iomsg=msg)     myunit, b1(myunit), b2(b1(myunit)%c), b3( b1(myunit)%c, b2(b1(myunit)%c)%c )      ! write 1,2,7,21
   write (myunit, iostat=stat, iomsg=msg)     2,3,8,200


   rewind 1

   read  (myunit, iostat=stat, iomsg=msg)     result

   ! read unit 1, myunit = 2, b1(2) = 3+1, b2(3)=8+1, b3(3,8) = 200+1
   read  (myunit, iostat=stat, iomsg=msg)     myunit, b1(myunit), b2(b1(myunit)%c), b3( b1(myunit)%c, b2(b1(myunit)%c)%c )


   ! check if the results are set correctly

   if ( ( result(1) /= 1 ) .or. ( result(2) /= 2 ) .or. ( result(3) /= 7 ) .or. ( result(4) /= 21 ) ) error stop 1_4
   if ( ( myunit /= 2 ) .or. ( b1(2)%c /= 4 ) .or. ( b2(4)%c /= 9 ) .or. ( b3(4,9)%c /= 201 ))        error stop 2_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   integer :: temp

   read (unit, iostat=iostat ) temp

   call dtv%setC(temp+1)

   iomsg = 'dtio'

end subroutine

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, iomsg=iomsg, iostat=iostat ) dtv%getC()+1

end subroutine