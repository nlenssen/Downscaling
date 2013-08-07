%
%
%

ddir = '/ptmp/thoar/downscaling';

fnames = {'pr_A1.20C3M_3.CCSM.atmm.1870-01_cat_1879-12.downscaled.nc',
          'pr_A1.20C3M_3.CCSM.atmm.1880-01_cat_1889-12.downscaled.nc',
          'pr_A1.20C3M_3.CCSM.atmm.1890-01_cat_1899-12.downscaled.nc',
          'pr_A1.20C3M_3.CCSM.atmm.1900-01_cat_1909-12.downscaled.nc',
          'pr_A1.20C3M_3.CCSM.atmm.1910-01_cat_1919-12.downscaled.nc',
          'pr_A1.20C3M_3.CCSM.atmm.1920-01_cat_1929-12.downscaled.nc',
          'pr_A1.20C3M_3.CCSM.atmm.1930-01_cat_1939-12.downscaled.nc',
          'pr_A1.20C3M_3.CCSM.atmm.1940-01_cat_1949-12.downscaled.nc',
          'pr_A1.20C3M_3.CCSM.atmm.1950-01_cat_1959-12.downscaled.nc',
          'pr_A1.20C3M_3.CCSM.atmm.1960-01_cat_1969-12.downscaled.nc',
          'pr_A1.20C3M_3.CCSM.atmm.1970-01_cat_1979-12.downscaled.nc',
          'pr_A1.20C3M_3.CCSM.atmm.1980-01_cat_1989-12.downscaled.nc',
          'pr_A1.20C3M_3.CCSM.atmm.1990-01_cat_1999-12.downscaled.nc'};

fnames = {'ppt_A1.PDcntrl_1.CCSM.atmm.100-01_cat_109-12.downscaled.nc',
          'ppt_A1.PDcntrl_1.CCSM.atmm.110-01_cat_119-12.downscaled.nc',
          'ppt_A1.PDcntrl_1.CCSM.atmm.120-01_cat_129-12.downscaled.nc',
          'ppt_A1.PDcntrl_1.CCSM.atmm.130-01_cat_139-12.downscaled.nc',
          'ppt_A1.PDcntrl_1.CCSM.atmm.140-01_cat_149-12.downscaled.nc',
          'ppt_A1.PDcntrl_1.CCSM.atmm.150-01_cat_159-12.downscaled.nc',
          'ppt_A1.PDcntrl_1.CCSM.atmm.160-01_cat_169-12.downscaled.nc',
          'ppt_A1.PDcntrl_1.CCSM.atmm.170-01_cat_179-12.downscaled.nc',
          'ppt_A1.PDcntrl_1.CCSM.atmm.180-01_cat_189-12.downscaled.nc',
          'ppt_A1.PDcntrl_1.CCSM.atmm.190-01_cat_199-12.downscaled.nc',
          'ppt_A1.PDcntrl_1.CCSM.atmm.200-01_cat_209-12.downscaled.nc',
          'ppt_A1.PDcntrl_1.CCSM.atmm.210-01_cat_219-12.downscaled.nc',
          'ppt_A1.PDcntrl_1.CCSM.atmm.220-01_cat_229-12.downscaled.nc',
          'ppt_A1.PDcntrl_1.CCSM.atmm.230-01_cat_239-12.downscaled.nc',
          'ppt_A1.PDcntrl_1.CCSM.atmm.240-01_cat_249-12.downscaled.nc',
          'ppt_A1.PDcntrl_1.CCSM.atmm.250-01_cat_259-12.downscaled.nc',
          'ppt_A1.PDcntrl_1.CCSM.atmm.260-01_cat_269-12.downscaled.nc',
          'ppt_A1.PDcntrl_1.CCSM.atmm.270-01_cat_279-12.downscaled.nc',
          'ppt_A1.PDcntrl_1.CCSM.atmm.280-01_cat_289-12.downscaled.nc',
          'ppt_A1.PDcntrl_1.CCSM.atmm.290-01_cat_299-12.downscaled.nc',
          'ppt_A1.PDcntrl_1.CCSM.atmm.300-01_cat_309-12.downscaled.nc',
          'ppt_A1.PDcntrl_1.CCSM.atmm.310-01_cat_319-12.downscaled.nc',
          'ppt_A1.PDcntrl_1.CCSM.atmm.320-01_cat_329-12.downscaled.nc',
          'ppt_A1.PDcntrl_1.CCSM.atmm.330-01_cat_339-12.downscaled.nc',
          'ppt_A1.PDcntrl_1.CCSM.atmm.340-01_cat_349-12.downscaled.nc',
          'ppt_A1.PDcntrl_1.CCSM.atmm.350-01_cat_359-12.downscaled.nc',
          'ppt_A1.PDcntrl_1.CCSM.atmm.360-01_cat_369-12.downscaled.nc',
          'ppt_A1.PDcntrl_1.CCSM.atmm.370-01_cat_379-12.downscaled.nc',
          'ppt_A1.PDcntrl_1.CCSM.atmm.380-01_cat_389-12.downscaled.nc',
          'ppt_A1.PDcntrl_1.CCSM.atmm.390-01_cat_399-12.downscaled.nc',
          'ppt_A1.PDcntrl_1.CCSM.atmm.400-01_cat_409-12.downscaled.nc',
          'ppt_A1.PIcntrl_1.CCSM.atmm.280-01_cat_289-12.downscaled.nc',
          'ppt_A1.PIcntrl_1.CCSM.atmm.290-01_cat_299-12.downscaled.nc',
          'ppt_A1.PIcntrl_1.CCSM.atmm.300-01_cat_309-12.downscaled.nc',
          'ppt_A1.PIcntrl_1.CCSM.atmm.310-01_cat_319-12.downscaled.nc',
          'ppt_A1.PIcntrl_1.CCSM.atmm.320-01_cat_329-12.downscaled.nc',
          'ppt_A1.PIcntrl_1.CCSM.atmm.330-01_cat_339-12.downscaled.nc',
          'ppt_A1.PIcntrl_1.CCSM.atmm.340-01_cat_349-12.downscaled.nc',
          'ppt_A1.PIcntrl_1.CCSM.atmm.350-01_cat_359-12.downscaled.nc',
          'ppt_A1.PIcntrl_1.CCSM.atmm.360-01_cat_369-12.downscaled.nc',
          'ppt_A1.PIcntrl_1.CCSM.atmm.370-01_cat_379-12.downscaled.nc',
          'ppt_A1.PIcntrl_1.CCSM.atmm.380-01_cat_389-12.downscaled.nc',
          'ppt_A1.PIcntrl_1.CCSM.atmm.390-01_cat_399-12.downscaled.nc',
          'ppt_A1.PIcntrl_1.CCSM.atmm.400-01_cat_409-12.downscaled.nc',
          'ppt_A1.PIcntrl_1.CCSM.atmm.410-01_cat_419-12.downscaled.nc',
          'ppt_A1.PIcntrl_1.CCSM.atmm.420-01_cat_429-12.downscaled.nc',
          'ppt_A1.PIcntrl_1.CCSM.atmm.430-01_cat_439-12.downscaled.nc',
          'ppt_A1.PIcntrl_1.CCSM.atmm.440-01_cat_449-12.downscaled.nc',
          'ppt_A1.PIcntrl_1.CCSM.atmm.450-01_cat_459-12.downscaled.nc',
          'ppt_A1.PIcntrl_1.CCSM.atmm.460-01_cat_469-12.downscaled.nc',
          'ppt_A1.PIcntrl_1.CCSM.atmm.470-01_cat_479-12.downscaled.nc',
          'ppt_A1.PIcntrl_1.CCSM.atmm.480-01_cat_489-12.downscaled.nc',
          'ppt_A1.PIcntrl_1.CCSM.atmm.490-01_cat_499-12.downscaled.nc',
          'ppt_A1.PIcntrl_1.CCSM.atmm.500-01_cat_509-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.300-01_cat_309-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.310-01_cat_319-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.320-01_cat_329-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.330-01_cat_339-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.340-01_cat_349-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.350-01_cat_359-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.360-01_cat_369-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.370-01_cat_379-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.380-01_cat_389-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.390-01_cat_399-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.400-01_cat_409-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.410-01_cat_419-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.420-01_cat_429-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.430-01_cat_439-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.440-01_cat_449-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.450-01_cat_459-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.460-01_cat_469-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.470-01_cat_479-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.480-01_cat_489-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.490-01_cat_499-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.500-01_cat_509-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.510-01_cat_519-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.520-01_cat_529-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.530-01_cat_539-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.540-01_cat_549-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.550-01_cat_559-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.560-01_cat_569-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.570-01_cat_579-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.580-01_cat_589-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.590-01_cat_599-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.600-01_cat_609-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.610-01_cat_619-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.620-01_cat_629-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.630-01_cat_639-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.640-01_cat_649-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.650-01_cat_659-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.660-01_cat_669-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.670-01_cat_679-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.680-01_cat_689-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.690-01_cat_699-12.downscaled.nc',
          'ppt_A1.PIcntrl_2.CCSM.atmm.700-01_cat_709-12.downscaled.nc'};

clf
orient landscape

for  ifile = 1:length(fnames)

   str1 = fnames{ifile};
   experiment = str1(1:(findstr(str1,'.nc')-1));

   fname = sprintf('%s/%s',ddir,fnames{ifile});

   if ( exist(fname,'file') == 2 ) 
      disp(sprintf('working on %s',fnames{ifile}))
   else
      disp(sprintf('%s does not exist ... next ...',fnames{ifile}))
      continue
   end

   lons  = getnc(fname,'lon');
   lats  = getnc(fname,'lat');
   times = getnc(fname,'time') + datenum(1601,1,1);

   if (length(times) ~= 120 ) 
      error(sprintf('%s only had %d timesteps',fnames{ifile},length(times)))
   end
   
   start = [ 1,417,1320];
   endpt = [-1,419,1322];
   
   disp('getting cape cod region ...')
   bob = getnc(fname,'ppt',start,endpt);
   Tdatmat = squeeze(sum(bob,1));
   missing = [Tdatmat(1,2) Tdatmat(1,3) Tdatmat(2,:) Tdatmat(3,1) Tdatmat(3,3)];
   valid   = [Tdatmat(1,1) Tdatmat(3,2)];
   nvalid  = sum(isfinite(bob(:)))

   if ( nvalid ~= 240)
      error(sprintf('%s failed cape code region length %d',fnames{ifile},nvalid))
   end
   if any(isfinite(missing)) 
      error(sprintf('%s failed cape code region',fnames{ifile}))
   end
   if ( any(isnan(valid)) || any(isinf(valid)))
      error(sprintf('%s FAILED cape code region',fnames{ifile}))
   end
   
   subplot(3,1,1)
   plot(times,bob(:,1,1), ...
        times,bob(:,3,2) )
%  axis([-Inf Inf 250 302])
   title(fname,'interpreter','none');
   
   disp('getting mountain region ...')
   start = [ 1,400,400];
   endpt = [-1,400,400];
   mtn   = getnc(fname,'ppt',start,endpt);
   subplot(3,1,2)
   plot(times, mtn)
   title('mountains')
   if (sum(isfinite(mtn)) ~= length(mtn)) 
      error(sprintf('%s FAILED mountain region %d %d',sum(isfinite(mtn)),length(mtn)))
   end
%  axis([-Inf Inf 250 302])
   
   disp('getting buffalo region ...')
   start = [ 1,456,1103];
   endpt = [-1,458,1103];
   bflo  = getnc(fname,'ppt',start,endpt);
   subplot(3,1,3)
   plot(times, bflo )
   title('buffalo')
   if (sum(isfinite(bflo)) ~= length(bflo)) 
      error(sprintf('%s FAILED Buffalo region %d %d',sum(isfinite(bflo)),length(bflo)))
   end
%  axis([-Inf Inf 250 302])

   psfname = sprintf('%s.pdf',experiment);
   print('-dpdf',psfname)

end
