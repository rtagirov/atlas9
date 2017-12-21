!--------------------------------------------------------------
! ReadNetCDF:
!         Open a netcdf file, dimension the arrays and 
!         put it into data mode
!--------------------------------------------------------------
! Complication as the leading dimension is padded to allow
! for increased performance on the IBM
! 
!--------------------------------------------------------------
!--------------------------------------------------------------
!--------------------------------------------------------------
!--------------------------------------------------------------
subroutine ReadContinum ( myrank, comm, nproc, ncid,        &
                        wledge, freqedge, cmedge, idmol, momass, freqset, &
                        temp, pressure, t4, rho, xne, xnatom, vturb, xnfh24, xnfhe4, &
                        xnfh4, contall, contsca, xnfpel4, dopmax, dopple4,  &
                        nrhox, ntemp, nufreq, ier)


!
      implicit none
!
      include 'netcdf.inc'
#ifdef MPI
      include 'mpif.h'
#endif

      integer,intent(out)  :: ier
      integer              :: myrank, nproc
      integer              :: comm

      integer              :: ncid
      


      integer          status

      integer          nrhoxid, ntempid, nufreqid
      integer          nrhox, ntemp, nufreq, nufreq3 


      double precision  wledge(nufreq/3), freqedge(nufreq/3), cmedge(nufreq/3)
      double precision  idmol(60), momass(60)
      double precision  freqset(nufreq)
      double precision  temp(ntemp), pressure(nrhox)

      double precision  t4(nrhox), rho(nrhox)
      double precision  xne(nrhox), xnatom(nrhox)
      double precision  vturb(nrhox), xnfh24(nrhox)
      double precision  xnfhe4(nrhox,  2), xnfh4(nrhox, 2)
!
      double precision  contall(nufreq, nrhox), contsca(nufreq, nrhox)
      double precision  xnfpel4(6, 99, nrhox), dopmax(nufreq/3, 6, 99)
      double precision  dopple4(6, 99, nrhox)
      integer          istart1,  istart2(2), istart3(3), istart4(4)
      integer          icount1,  icount2(2), icount3(3), icount4(4)
      integer           i,j,k,l,m

      integer          varid



      ier = 0
!-----------------------------------------------------------------
      if ( myrank .eq. 0 ) then
!-----------------------------------------------------------------

!
!.......Read the dimension id's
! nf_inq_dimid returns dimension ID (integer) of "string" in ncid

      status = nf_inq_dimid( ncid, 'nrhox', nrhoxid )
      if ( status .ne. NF_NOERR ) call  handle_error(status)
      status = nf_inq_dimid( ncid, 'ntemp', ntempid )
      if ( status .ne. NF_NOERR ) call  handle_error(status)
      status = nf_inq_dimid( ncid, 'nufreq', nufreqid )
      if ( status .ne. NF_NOERR ) call  handle_error(status)

!.......Read the actual dimensions 
! nf_inq_dimlen returns length of dimension
!  
      status = nf_inq_dimlen( ncid, nrhoxid, nrhox )
      if ( status .ne. NF_NOERR ) call  handle_error(status)
      status = nf_inq_dimlen( ncid, ntempid, ntemp )
      if ( status .ne. NF_NOERR ) call  handle_error(status)
      status = nf_inq_dimlen( ncid, nufreqid, nufreq )
      if ( status .ne. NF_NOERR ) call  handle_error(status)

!
!.......Paranoid check
!

!----------- left out I am not paranoid ...

!-----------------------------------------------------------------
      end if
!-----------------------------------------------------------------



!======================================================================================


   if (nproc .eq. 1) then 


         status = nf_inq_varid( ncid, 'mID', varid )
         if ( status .ne. NF_NOERR ) call  handle_error(status)
         status = nf_get_vara_double(ncid, varid, 1, 60, idmol(1))

         status = nf_inq_varid( ncid, 'mmass', varid )
         if ( status .ne. NF_NOERR ) call  handle_error(status)
         status = nf_get_vara_double(ncid, varid, 1, 60, momass(1))

         status = nf_inq_varid( ncid, 'frqed', varid )
         if ( status .ne. NF_NOERR ) call  handle_error(status)
         status = nf_get_vara_double(ncid, varid, 1, 377, freqedge(1))

         status = nf_inq_varid( ncid, 'wled', varid )
         if ( status .ne. NF_NOERR ) call  handle_error(status)
         status = nf_get_vara_double(ncid, varid, 1, 377, wledge(1))

         status = nf_inq_varid( ncid, 'cmed', varid )
         if ( status .ne. NF_NOERR ) call  handle_error(status)
         status = nf_get_vara_double(ncid, varid, 1, 377, cmedge(1))

         status = nf_inq_varid( ncid, 'frqset', varid )
         if ( status .ne. NF_NOERR ) call  handle_error(status)
         status = nf_get_vara_double(ncid, varid, 1, nufreq, freqset(1))

         status = nf_inq_varid( ncid, 'pressure', varid )
         if ( status .ne. NF_NOERR ) call  handle_error(status)
         status = nf_get_vara_double(ncid, varid, 1, nrhox, pressure(1))

         status = nf_inq_varid( ncid, 'temp', varid )
         if ( status .ne. NF_NOERR ) call  handle_error(status)
         status = nf_get_vara_double(ncid, varid, 1, ntemp, temp(1))



   endif  

         istart1 = 1+(nrhox*(nproc-1))
         icount1 = nrhox

         status = nf_inq_varid( ncid, 't4', varid )
         if ( status .ne. NF_NOERR ) call  handle_error(status)
         status = nf_get_vara_double(ncid, varid, istart1, icount1, t4)


          status = nf_inq_varid( ncid, 'rho', varid )
          if ( status .ne. NF_NOERR) call handle_error(status)
          status = nf_get_vara_double(ncid,varid,istart1, icount1, rho  )
          if ( status .ne. NF_NOERR) call handle_error(status)


          status = nf_inq_varid( ncid, 'Xne', varid )
          if ( status .ne. NF_NOERR) call handle_error(status)
          status = nf_get_vara_double(ncid,varid,istart1, icount1, xne )
          if ( status .ne. NF_NOERR) call handle_error(status)


          status = nf_inq_varid( ncid, 'xnatom', varid )
          if ( status .ne. NF_NOERR) call handle_error(status)
          status = nf_get_vara_double(ncid,varid,istart1, icount1, xnatom )
          if ( status .ne. NF_NOERR) call handle_error(status)



          status = nf_inq_varid( ncid, 'vturb', varid )
          if ( status .ne. NF_NOERR) call handle_error(status)
          status = nf_get_vara_double(ncid,varid,istart1, icount1, vturb )
          if ( status .ne. NF_NOERR) call handle_error(status)


          status = nf_inq_varid( ncid, 'xnfh24', varid )
          if ( status .ne. NF_NOERR) call handle_error(status)
          status = nf_get_vara_double(ncid,varid,istart1, icount1, xnfh24 )
          if ( status .ne. NF_NOERR) call handle_error(status)

   


         do i=1, 2

       istart2(1) = 1+(nrhox*(nproc-1))
       istart2(2) = i
       icount2(1) = nrhox
       icount2(2) = 1

          status = nf_inq_varid( ncid, 'xnfh4', varid )
          if ( status .ne. NF_NOERR) call handle_error(status)
          status = nf_get_vara_double(ncid,varid,istart2, icount2, xnfh4(1,i) )
          if ( status .ne. NF_NOERR) call handle_error(status)


          status = nf_inq_varid( ncid, 'xnfhe4', varid )
          if ( status .ne. NF_NOERR) call handle_error(status)
          status = nf_get_vara_double(ncid,varid,istart2, icount2, xnfhe4(1,i) )
          if ( status .ne. NF_NOERR) call handle_error(status)
         end do

       do i = 1, nrhox
       istart2(1) = 1
       istart2(2) = i+(nrhox*(nproc-1))
       icount2(1) = nufreq
       icount2(2) = 1

          status = nf_inq_varid( ncid, 'contall', varid )
          if ( status .ne. NF_NOERR) call handle_error(status)
          status = nf_get_vara_double(ncid,varid,istart2, icount2, contall(1,i) )
          if ( status .ne. NF_NOERR) call handle_error(status)


          status = nf_inq_varid( ncid, 'contsca', varid )
          if ( status .ne. NF_NOERR) call handle_error(status)
          status = nf_get_vara_double(ncid,varid,istart2, icount2, contsca(1,i) )
          if ( status .ne. NF_NOERR) call handle_error(status)

        end do





       do i = 1, nrhox
         do j = 1, 99

         istart3(1) = 1
         istart3(2) = j
         istart3(3) = i+ (nrhox*(nproc-1))
         icount3(1) = 6
         icount3(2) = 1
         icount3(3) = 1


          status = nf_inq_varid( ncid, 'xnfpel4', varid )
          if ( status .ne. NF_NOERR) call handle_error(status)
          status = nf_get_vara_double(ncid,varid,istart3, icount3, xnfpel4(1,j,i) )
          if ( status .ne. NF_NOERR) call handle_error(status)


          status = nf_inq_varid( ncid, 'dopple', varid )
          if ( status .ne. NF_NOERR) call handle_error(status)
          status = nf_get_vara_double(ncid,varid,istart3, icount3, dopple4(1,j,i) )
          if ( status .ne. NF_NOERR) call handle_error(status)



         end do
      end do



          do j = 1, 99 
             do l= 1, 6
         istart4(1) = 1
         istart4(2) = l
         istart4(3) = j
         istart4(4) = nproc
         icount4(1) = nufreq3 
         icount4(2) = 1
         icount4(3) = 1
         icount4(4) = 1


          status = nf_inq_varid( ncid, 'dopmax', varid )
          if ( status .ne. NF_NOERR) call handle_error(status)

          status = nf_get_vara_double(ncid,varid,istart4, icount4, dopmax(1,l,j) )
          if ( status .ne. NF_NOERR) call handle_error(status)

           end do
        end do



        !---------------------
        ! Read data as doubles
        !---------------------

!         write(*,*) 'READING R'
!         status = nf_inq_varid( ncid, 'R', varid )
!         if ( status .ne. NF_NOERR ) call  handle_error(status)
!         do k=1,nlayer
!           do j=1,ny
!
!             istart(1) = 1
!             istart(2) = j
!             istart(3) = k

!             icount(1) = nx
!             icount(2) = 1
!             icount(3) = 1

!             status = nf_get_vara_double(ncid, varid, istart, icount, r(1,j,k))
!           end do
!         end do


      print *,'Finished Reading NetCDF file'

!======================================================================================

     return 

end subroutine ReadContinum 

