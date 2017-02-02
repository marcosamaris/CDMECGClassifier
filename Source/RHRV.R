##Reading a wfdb register and storing into a data structure:
md = CreateHRVData(Verbose = TRUE)
md = LoadBeatWFDB(md, RecordName = "register_name", RecordPath = "register_path")
##Loading information of episodes of apnea:
md = LoadApneaWFDB(md, RecordName = "register_name", RecordPath = "register_path", Tag= "APN")
##Generating new episodes before and after previous episodes of apnea:
md = GenerateEpisodes(md, NewBegFrom = "Beg", NewEndFrom = "Beg", DispBeg = -600, DispEnd = -120, OldTag = "APN", NewTag = "PREV_APN")
md = GenerateEpisodes(md, NewBegFrom = "End", NewEndFrom = "End", DispBeg = 120, DispEnd = 600, OldTag = "APN", NewTag = "POST_APN")
##Calculating heart rate signal:
md = BuildNIHR(md)
##Filtering heart rate signal:
md = FilterNIHR(md)
##Interpolating heart rate signal:
md = InterpolateNIHR(md)
##Calculating spectrogram and power per band:
md = CreateFreqAnalysis(md)
md = CalculatePowerBand(md, indexFreqAnalysis = 1, size = 120, shift = 10, sizesp = 1024)
##Plotting power per band, including episodes information:
PlotPowerBand(md, indexFreqAnalysis = 1, hr = TRUE, ymax = 2400000, ymaxratio = 3, Tag ="all")
##Splitting power per band using episodes before and after episodes of apnea:
PrevAPN = SplitPowerBandByEpisodes(md, indexFreqAnalysis = 1, Tag = "PREV_APN")
PostAPN = SplitPowerBandByEpisodes(md, indexFreqAnalysis = 1, Tag = "POST_APN")
##Performing Student’s t-test:
result = t.test(PrevAPN$InEpisodes$ULF, PostAPN$InEpisodes$ULF)
print(result)