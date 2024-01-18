!*    ********************************************************************
!*                            KMEANS ROUTINE                             *
!*    ********************************************************************

      subroutine kmeans (N,K,D,pointv,clusterv,niters)
      use timers_module
      implicit none
      integer*8 N,K,D,niters,i,chg,k1
      double precision pointv(N,D)[*]
      double precision clusterv(K,D)
      integer*8 lnearestv(N,D),counterv(K),oldcounterv(K)

      interface
         subroutine kmeans_classify(N,K,D,pointv,lnearestv,clusterv)
           integer*8 N, K, D
           double precision pointv(N,D)[*]
           double precision clusterv(K,D)
           double precision classify
           integer*8 lnearestv(N)
         end subroutine
         subroutine kmeans_reaverage(N,K,D,pointv,lnearestv,clusterv,ctrv)
           integer*8 N, K, D
           double precision pointv(N,D)[*]
           double precision clusterv(K,D)
           integer*8 lnearestv(N),ctrv(K)
         end subroutine
         integer*8 function kmeans_getchange(K,counterv,oldcounterv)
           integer*8 K,counterv(K),oldcounterv(K)
         end function
      end interface

      oldcounterv(:)=0
      counterv(:)=0
      
      call timer_start(10)
      do i=1,niters
!         call dump (K,D,clusterv,counterv,oldcounterv)
         call timer_start(1)
         call kmeans_classify(N,K,D,pointv,lnearestv,clusterv)
         call timer_stop(1)
         call timer_start(2)
         call kmeans_reaverage(N,K,D,pointv,lnearestv,clusterv,counterv)
         call timer_stop(2)
         if (this_image() .EQ. 1) then
            chg = kmeans_getchange(K,counterv,oldcounterv)
            print *, 'Iter=', i, ' chg=', chg 
         end if
      end do
      call timer_stop(10)
      end


!*    ********************************************************************
!*                       REAVERAGE KERNEL                                *
!*    ********************************************************************

      subroutine kmeans_reaverage(N,K,D,pointv,lnearestv,clusterv,counterv)
      implicit none
      integer*8 N, K, D, p, k1, n1, d1
      double precision pointv(N,D)[*]
      double precision clusterv(K,D), rctr
      integer*8 counterv(K)
      integer*8 lnearestv(N)

      counterv(:)=0
      clusterv(:,:)=0.0

      do n1=1,N
         p = lnearestv(n1)
         if ((p .LT. 1) .OR. (p .GT. K)) then
            print *, 'Error: ', n1, '--->', p
         end if
         counterv(p) = counterv(p)+1
         clusterv(p,:)=clusterv(p,:)+pointv(n1,:)
      end do

      call kmeans_reduce_dbl (clusterv(1,1),K*D)
      call kmeans_barrier ()
      call kmeans_reduce_int (counterv(1),K)

      do k1=1,K
         if (counterv(k1) .NE. 0) then
            rctr = 1.0 / counterv(k1)
            clusterv(k1,:) = clusterv(k1,:)*rctr
         end if
      end do

      end


!*    ********************************************************************
!*    *               calculate the change and update counters           *
!*    ********************************************************************

      integer*8 function kmeans_getchange (K,counterv, oldcounterv)
      implicit none
      integer*8 counterv(K),oldcounterv(K),chg,K
      chg = sum(abs(counterv(:)-oldcounterv(:)))
      oldcounterv(:)=counterv(:)
      kmeans_getchange = chg
      end function

!*    ********************************************************************
!*      calculate the maximum distance between any point and its centroid
!*    ********************************************************************

      double precision function kmeans_maxdist (N,K,D,pointv,clusterv)
      interface
         subroutine kmeans_classify(N,K,D,pointv,lnearestv,clusterv)
           integer*8 N, K, D
           double precision pointv(N,D)[*]
           double precision clusterv(K,D)
           double precision classify
           integer*8 lnearestv(N)
         end subroutine
      end interface
      implicit none
      integer*8 N, K, D
      double precision pointv(N,D)[*]
      double precision clusterv(K,D)
      integer*8 lnearestv(N)
      integer*8 n1,k1,d1,one
      double precision maxdist,d0,diff0

      call kmeans_classify (N,K,D,pointv,lnearestv,clusterv)

      maxdist = 0.0
      do n1=1,N
         k1 = lnearestv(n1)
         d0 = 0.0
         do d1=1,D
            diff0 = pointv(n1,d1)-clusterv(k1,d1)
            d0 = d0 + diff0*diff0
         end do
         d0 = sqrt(d0)
         if (d0 .gt. maxdist) then
            maxdist = d0
         end if
      end do
      one = 1
      call kmeans_reduce_max (maxdist,one)
      kmeans_maxdist = maxdist
      end

#if 1

!*
!*           temporary stuff
!*

      subroutine dump(K,D,clusterv,counterv,oldcounterv)
      integer*8 K,D,d1,k1,counterv(K),oldcounterv(K)
      double precision clusterv(K,D)
      
      do k1=1,K
         do d1=1,D
            write(*, 10, advance='no') clusterv(k1,d1)
 10         format (d15.10, ' ')
         end do
         print *, ' ', counterv(k1), ' ', oldcounterv(k1)
      end do
      end

#endif
