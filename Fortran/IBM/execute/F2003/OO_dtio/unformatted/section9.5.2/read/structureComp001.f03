! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try input item to be structure component
!*                               Sequential Access
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
      character(3) :: c = ''
      contains
         procedure, pass :: getC
   end type

   type container
      class(base), pointer     :: b1
      class(base), allocatable :: b2
   end type

contains
   function getC (a)
      class(base), intent(in) :: a
      character(3) :: getC
      getC = a%c
   end function
end module


program structureComp001
   use m1

   interface read(unformatted)
      subroutine readUnformattedContainer (dtv, unit, iostat, iomsg)
         import container
         class(container), intent(inout) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables
   class(container), allocatable  :: b11
   class(container), pointer      :: b12
   type (container)               :: b13
   integer :: stat
   character(200) :: msg

   ! allocation of variables

   allocate ( b11 )
   allocate ( b12 )

   allocate ( b11%b1, source = base('xxx') )
   allocate ( b11%b2, source = base('xxx') )
   allocate ( b12%b1, source = base('xxx') )
   allocate ( b12%b2, source = base('xxx') )
   allocate ( b13%b1, source = base('xxx') )
   allocate ( b13%b2, source = base('xxx') )

   open (unit = 1, file ='structureComp001.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )             'abcdef'
   write (1, iostat=stat, iomsg=msg )             'ghijkl'
   write (1, iostat=stat, iomsg=msg )             'mnopqr'

   rewind 1

   read (1, iostat=stat, iomsg=msg )             b11        !<- write 'abcdef' to file
      if ( ( stat /= 0 ) .or. ( msg /= 'containerdtio' ) )    error stop 1_4
      msg = ''
   read (1, iostat=stat, iomsg=msg )             b12        !<- write 'ghijkl' to file
      if ( ( stat /= 0 ) .or. ( msg /= 'containerdtio' ) )    error stop 2_4
      msg = ''
   read (1, iostat=stat, iomsg=msg )             b13        !<- write 'mnopqr' to file
      if ( ( stat /= 0 ) .or. ( msg /= 'containerdtio' ) )    error stop 3_4
      msg = ''

   ! check if the values are set correctly

   if ( ( b11%b1%c /= 'abc' ) .or. ( b11%b2%c /= 'def' ) )           error stop 4_4
   if ( ( b12%b1%c /= 'ghi' ) .or. ( b12%b2%c /= 'jkl' ) )           error stop 5_4
   if ( ( b13%b1%c /= 'mno' ) .or. ( b13%b2%c /= 'pqr' ) )           error stop 6_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readUnformattedContainer (dtv, unit, iostat, iomsg)
use m1
    class(container), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    interface read(unformatted)
        subroutine readUnformattedBase (dtv, unit, iostat, iomsg)
            import base
            class(base), intent(inout) :: dtv
            integer,  intent(in) :: unit
            integer,  intent(out) :: iostat
            character(*),  intent(inout) :: iomsg
        end subroutine
    end interface


    read (unit, iostat=iostat, iomsg=iomsg ) dtv%b1, dtv%b2

    if ( ( iostat /= 0 ) .or. ( iomsg /= 'basedtio' ) )    error stop 7_4

    iomsg = 'containerdtio'

end subroutine

subroutine readUnformattedBase (dtv, unit, iostat, iomsg)
use m1
    class(base), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    read (unit, iostat=iostat, iomsg=iomsg ) dtv%c

    iomsg = 'basedtio'

end subroutine

