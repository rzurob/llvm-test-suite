!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : saBasicStatusNMsg
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2010-07-26
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF SYNC ALL
!*  SECONDARY FUNCTIONS TESTED : images complete, status is 0, message is not overwritten on success
!*  REFERENCE                  : Feature Number 379697
!*  REQUIRED COMPILER OPTIONS  : -qcaf -qcaf=images=<num>
!*  ADAPTED FROM               : saBasicMessage, saBasicStatus ()
!*
!*  DESCRIPTION
!*
!*  Verify that the errmsg variable remains unchanged and the status variable is
!*  0 on successful completion of SYNC ALL.
!*  Also, to test the coexistence of SYNC ALL statements with and without sync-stat options,
!*  we include a second SYNC ALL statement.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program saBasicStatusNMsg

   implicit none

   integer, parameter :: FILL_LENGTH = 100
   character(FILL_LENGTH), parameter :: fill = repeat('Z',FILL_LENGTH)
   character(FILL_LENGTH) :: emsg
   integer :: self, nImages, out, status
   character(4) :: outfile

   emsg = fill

   self = this_image()
   nImages = num_images()

   ! create log file for each image
   out = self+10
   write(outfile,'("o",i3.3)') out
   open(out,file=outfile,action='WRITE',status='REPLACE')

   write(out,*) 'Before'

   sync all (stat=status,errmsg=emsg)

   write(out,*) 'After'

   if (status /= 0) write(out,*) 'Non-zero status:', status
   if (emsg /= fill) write(out,*) 'Altered errmsg:', emsg

   flush(out)
   close(out)

   if (self == 1) then
      print *, nImages
   end if

   sync all

   if (status /= 0 .or. emsg /= fill) stop 2

end program saBasicStatusNMsg
