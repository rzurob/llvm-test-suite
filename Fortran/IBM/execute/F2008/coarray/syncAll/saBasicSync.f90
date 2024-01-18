!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : saBasicSync
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2010-07-26
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF SYNC ALL
!*  SECONDARY FUNCTIONS TESTED : basic test of synchronisation
!*  REFERENCE                  : Feature Number 379697
!*  REQUIRED COMPILER OPTIONS  : -qcaf -qcaf=images=<num>
!*  ADAPTED FROM               : saBasic ()
!*
!*  DESCRIPTION
!*
!*  Verify that all images wait at the SYNC ALL until all have arrived there.
!*  This test uses a "buddy" system: each image notifies its "right" neighbour
!*  (the image with the next higher image number) just before the SYNC ALL
!*  statement that it has arrived at the SYNC ALL.  Immediately following the
!*  SYNC ALL, each image checks to see if its "left" neighbour has also arrived.
!*  If any are missing, the program exits with an error return code.
!*  ("buddy" is circular: the "right" buddy of the image with the highest number
!*  is image 1; the "left" buddy of image 1 is the image with the highest number.)
!*  To increase the likelihood of different arrival times, each waits a
!*  different (short) time before proceeding to the SYNC ALL.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program saBasicSync

   implicit none

   integer, save :: arrived[*]  ! coarray which contains the identity of every "arrived" image
   logical, save :: errorFound[*]
   integer :: self, rightBuddy, leftBuddy, nImages, i, aBuddy
   logical :: eFoundInImage, eFound
   
   
   arrived  = 0
   errorFound = .false.

   self = this_image()
   nImages = num_images()

   ! set ID of right buddy, and handle circular relations
   rightBuddy = self+1
   if (rightBuddy > nImages) rightBuddy = 1

   ! set ID of right buddy, and handle circular relations
   leftBuddy = self-1
   if (leftBuddy < 1) leftBuddy = nImages

   call sleep_(mod(self,4))

   ! tell the buddy on the right that we have arrived:
   arrived[rightBuddy] = self

   sync all

   ! check for our left buddy - error if it's not there
   aBuddy = arrived
   eFound = (aBuddy /= leftBuddy)
   errorFound = eFound

   sync all

   if (self == 1) then
      eFound = .false.
      do i = 1, nImages
         eFoundInImage = errorFound[i]
         if (eFoundInImage) then
            print *, "Error in image", i
            eFound = .true.
         end if
      end do
      if (eFound) then
         stop 2
      end if
   end if

end program saBasicSync
