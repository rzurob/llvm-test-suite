!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: structureComp001a.f
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
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try input item to be an structure component (without container interface)
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
      character(3) :: c = ''
      contains
         procedure, pass :: getC
   end type

   type container
      type(base) :: b1
      integer    :: i
      type(base) :: b2
   end type

contains
   function getC (a)
      class(base), intent(in) :: a
      character(3) :: getC
      getC = a%c
   end function
end module

program structureComp001a
   use m1

    interface read(unformatted)
        subroutine readUnformattedBase (dtv, unit, iostat, iomsg)
            import base
            class(base), intent(inout) :: dtv
            integer,  intent(in) :: unit
            integer,  intent(out) :: iostat
            character(*),  intent(inout) :: iomsg
        end subroutine
    end interface

   ! declaration of variables
   type(container), allocatable  :: b11
   type(container), pointer      :: b12
   type(container)               :: b13
   integer :: stat
   character(200) :: msg

   ! allocation of variables
   allocate ( b11, source = container( b2=base('xxx'), i=0, b1=base('xxx') ) )
   allocate ( b12, source = container( b2=base('xxx'), i=0, b1=base('xxx') ) )
   b13 = container( b2=base('xxx'), i=0, b1=base('xxx') )

   open (unit = 1, file ='structureComp001a.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )             'abc',1,'def'
   write (1, iostat=stat, iomsg=msg )             'ghi',2,'jkl'
   write (1, iostat=stat, iomsg=msg )             'mno',3,'pqr'

   rewind 1

   read (1, iostat=stat, iomsg=msg )             b11        !<- write 'abcdef' to file
      if ( ( stat /= 0 ) .or. ( msg /= 'basedtio' ) )    error stop 1_4
      msg = ''
   read (1, iostat=stat, iomsg=msg )             b12%b1      !<- write 'ghijkl' to file
      if ( ( stat /= 0 ) .or. ( msg /= 'basedtio' ) )    error stop 2_4
      msg = ''
   read (1, iostat=stat, iomsg=msg )             b13        !<- write 'mnopqr' to file
      if ( ( stat /= 0 ) .or. ( msg /= 'basedtio' ) )    error stop 3_4
      msg = ''

   if ( ( b11%b1%c /= 'abc' ) .or. ( b11%b2%c /= 'def' ) .or. (b11%i /= 1) )           error stop 4_4
   if ( ( b12%b1%c /= 'ghi' ) .or. ( b12%b2%c /= 'xxx' ) .or. (b12%i /= 0) )           error stop 5_4
   if ( ( b13%b1%c /= 'mno' ) .or. ( b13%b2%c /= 'pqr' ) .or. (b13%i /= 3) )           error stop 6_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readUnformattedBase (dtv, unit, iostat, iomsg)
use m1
    class(base), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    read (unit, iostat=iostat, iomsg=iomsg ) dtv%c

    iomsg = 'basedtio'

end subroutine
