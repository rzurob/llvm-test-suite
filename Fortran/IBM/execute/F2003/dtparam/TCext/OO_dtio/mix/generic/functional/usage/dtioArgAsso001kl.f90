!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtioArgAsso001kl
!*
!*  PROGRAMMER                 : David Forster (derived from dtioArgAsso001 by Robert Ma)
!*  DATE                       : 2007-08-13 (original: 04/26/2005)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Usage of GENERIC BINDING
!*                                  - Ensure DTIO procedure arguments are associated correctly
!*                                    in formatted i/o
!*                               adaptation: exposed length
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

   type base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c = 'xxx'
      contains
         procedure, pass :: write => writeb
         procedure, pass :: read => readb
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   character(30) :: buffer(10)
   integer :: idx

   contains

      subroutine writeb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write ( buffer(idx), *, iostat=iostat, iomsg=iomsg)  unit, iotype, v_list

         iomsg = 'dtiowriteb'
         
         idx = idx +1

      end subroutine

      subroutine readb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write ( buffer(idx), *, iostat=iostat, iomsg=iomsg)  unit, iotype, v_list
         iomsg = 'dtioreadb'
         
         idx = idx + 1

      end subroutine

end module

program dtioArgAsso001kl
   use m

   type(base(3))  :: b1 = base(3)() ! tcx: (3) ! tcx: (3)

   integer :: stat
   character(200) :: msg
   
   namelist /n1/ b1

   open ( 1, file = 'dtioArgAsso001kl.1', form='formatted', access='sequential' )

   idx = 1

   print *, b1        !<- unit: 6, iotype: "LISTDIRECTED", v_list: (//)
   write (*,*) b1     !<- unit: 6, iotype: "LISTDIRECTED", v_list: (//)
   write (1, n1)      !<- unit: 1, iotype: "NAMELIST", v_list: (//)
   write (1, "(DT'_b1'(1,2,3,4,5,6))")    b1   !<- unit: 1, iotype: "DT_b1", v_list: (/1,2,3,4,5,6/)

   rewind 1
   
   read (1, *, iostat = stat) b1                             !<- unit: 1, iotype: "LISTDIRECTED", v_list: (//)
   rewind 1
   read (1, n1, iostat = stat, iomsg = msg)                               !<- unit: 1, iotype: "NAMELIST", v_list: (//)
   read (1, "(DT'_b1'(1,2,3,4,5,6))", iostat = stat)    b1   !<- unit: 1, iotype: "DT_b1", v_list: (/1,2,3,4,5,6/)
   
   print *, buffer

   close ( 1, status ='delete')

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 4 changes
