!*  ===================================================================
!*
!*  TEST CASE NAME             : C464_006dkl
!*
!*  DATE                       : 2007-08-13 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Section 4.5.4 Type Bound Procedures (generic-binding)
!*                               Syntax Check: C464 If generic-spec is dtio-generic-spec, the interface of each binding
!*                                                  shall be as specified in 9.5.3.7. The type of the dtv argument
!*                                                  shall be type-name.
!*
!*                                             - Characteristics of the dummy arguments
!*                                                -> optional, and value attributes
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

   type ::  base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1

      character(lbase_1) :: c

      contains

         procedure, pass :: uread
         procedure, pass :: uwrite
         procedure, pass :: fread
         procedure, pass :: fwrite

         generic, public  :: read(unformatted)  => uread
         generic, public  :: write(unformatted) => uwrite
         generic, private :: read(formatted)    => fread
         generic, private :: write(formatted)   => fwrite

   end type

   contains

      subroutine uread (dtv, unit, iostat, iomsg)
         class(base(*)), intent(inout), optional :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat = iostat, iomsg = iomsg ) dtv%c

      end subroutine

      subroutine uwrite (dtv, unit, iostat, iomsg)
         class(base(*)), intent(in), value :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat = iostat, iomsg = iomsg ) dtv%c

      end subroutine

      subroutine fread (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in), value :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit,* ) dtv%c

      end subroutine

      subroutine fwrite (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in), optional :: unit
         character(*), intent(in), optional :: iotype
         integer, intent(in), optional  :: v_list(:)
         integer, intent(out), optional :: iostat
         character(*), intent(inout), optional :: iomsg

         write (unit,*, iostat = iostat, iomsg = iomsg ) dtv%c

      end subroutine

end module

program C464_006dkl
end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 4 changes
