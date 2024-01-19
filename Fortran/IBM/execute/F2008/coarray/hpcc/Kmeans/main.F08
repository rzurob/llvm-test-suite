!*    ********************************************************************
!*                               MAIN PROGRAM
!*    ********************************************************************

      program kmeans_main
      use timers_module
      implicit none
      integer*8 Nmax,Kmax,Dmax
      integer*8 N, K, D, niters
!      parameter(Nmax=8*1024*1024,Kmax=16384,Dmax=4)
      parameter(Nmax=1024*64,Kmax=16384,Dmax=4)
      integer*8, parameter :: megabyte = 2_8 ** 20
      character(100) argbuf

      double precision, save:: pointv (Nmax*Dmax)[*]
      double precision clusterv(Kmax,Dmax)
      double precision procGFlops, timePerIter, procBW
      double precision maxdist
      interface
         subroutine kmeans (N,K,D,lpointv,clusterv,niters)
           integer*8 N,K,D,niters
           double precision lpointv(N,D)[*]
           double precision clusterv(K,D)
         end subroutine
         subroutine randominit(N,K,D,lpointv,clusterv)
           integer*8 N,K,D
           double precision lpointv(N,D)[*]
           double precision clusterv(K,D)
         end subroutine
         double precision function kmeans_maxdist(N,K,D,pointv,clusterv)
           integer*8 N,K,D
           double precision pointv(N,D)[*]
           double precision clusterv(K,D)
         end function
      end interface

! ----------- read command line arguments -----------------------------

      call getarg(1, argbuf)
      read (argbuf,*) N
      call getarg(2, argbuf)
      read (argbuf,*) K
      call getarg(3, argbuf)
      read (argbuf,*) D
      call getarg(4, argbuf)
      read (argbuf,*) niters

! ---------- sanity check ----------------------------------------------

      if ((N .GT. Nmax) .OR. (K .GT. Kmax) .OR. (D .GT. Dmax)) then
         error stop "Parameters too large"
      end if

      if (this_image() .EQ. 1) then
         print *, 'k-means: N=', N, ' K=', K, ' D=', D
         print *, 'memory total=', N*D*8/1000000, 'MBs'
         print *, 'memory per image=', N*D*8/num_images()/1000000, 'MBs'
         print *, 'running on ' , num_images(), 'images'
      end if

      N = N / num_images()

! ---------- run iterations -------------------------------------------
      call timer_init()
      call randominit(N,K,D,pointv,clusterv)
      call kmeans(N,K,D,pointv,clusterv,niters)

! --------- verify ---------------------------------------------------
      maxdist = kmeans_maxdist(N,K,D, pointv,clusterv)

      if (this_image() .EQ. 1) then
 10      format('----------------------------------------')
 15      format('maximum distance       =', F8.4)
 20      format('Total time             =', F8.2, ' secs')
 30      format('   Classifier time     =', F8.2, ' secs')
 40      format('      Inner loop  time =', F5.2, ' ns')
 50      format('                  rate =', F5.2, ' GFlops')
 55      format('             equiv. BW =', F5.2, ' GBytes/s')
 60      format('   Reaverage time      =', F8.2, ' secs')

         timePerIter = timer_total(1) / K / N / D / niters * 1.0e9
         procGflops = 3.0/timePerIter
         procBW = 9.0/timePerIter

         write (*,10)
         write (*,15) maxdist
         write (*,10)
         write (*,20) timer_total(10)
         write (*,30) timer_total(1)
         write (*,40) timePerIter
         write (*,50) procGFlops
         write (*,55) procBW
         write (*,60) timer_total(2)
      end if

      end

!*    ********************************************************************
!*                       initialize with random numbers                  *
!*    ********************************************************************

      subroutine randominit(N0,K,D,lpointv,clusterv)
      use mtmod
      integer*8 N0,K,D,i,d1,k1
      double precision clusterv(K,D)
      double precision lpointv(N0,D)[*]

      call sgrnd(this_image())

      do i=1,N0
         do d1=1,D
            lpointv(i,d1)=grnd()
!            print *, lpointv(i,d1)
         end do
      end do

      do k1=1,K
         clusterv(k1,:) = lpointv(k1,:)
      end do

      end

