!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 04/26/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Final Subroutine
!*                                    -  Ensure final subroutine can be called during DTIO subroutine
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

   type base
      character(3) :: c = 'xxx'
      contains
         procedure, pass :: read => readb
         generic :: read(formatted) => read
         final :: finalbase
   end type

   type, extends(base) :: child
      integer(4) :: i = -999
      contains
         procedure, pass :: read => readc
         final :: finalchild
   end type

   character(30) :: buf(20)
   integer :: idx

   contains

      subroutine readb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         select type ( dtv )
            type is ( base )
               dtv = base('IBM')
         end select

         iomsg = 'dtioreadb'

      end subroutine

      subroutine readc (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         select type ( dtv )
            type is ( child )
               dtv = child('FTN',2003)
         end select

         iomsg = 'dtioreadc'

      end subroutine

      subroutine finalbase(dtv)
         type(base), intent(inout) :: dtv
         write ( buf(idx), * )  "inside fb: ", dtv%c
         idx = idx+1
      end subroutine

      subroutine finalchild(dtv)
         type(child), intent(inout) :: dtv
         write ( buf(idx), * )  "inside fc: ", dtv%c, dtv%i
         idx = idx+1
      end subroutine

end module

program final002
   use m

   integer :: stat
   character(200) :: msg

   type(base) :: b1 = base('abc')
   class(base), allocatable :: b2

   idx = 1

   open ( 1, file = 'final002.1', form='formatted', access='sequential' )
   allocate ( b2, source = child ( 'def', 101 ) )          !<- finalize child


   write ( 1, * ) "junkjunkjunk"
   write ( 1, * ) "junkjunkjunk"

   rewind 1

   read ( 1, * ) b1
   read ( 1, * ) b2

   print *, buf

end program

