saveRDS(read.csv("data/CMR_Registry_CRF_KHChoi.csv", check.names = F), "data/eCRFexam.RDS")


## RANDOM: 4 group
blocks <- rep(c("R-NDNST", "R-NDST", "R-DNST", "R-DST"), times = c(60, 60, 40, 40))
pid <- paste0(blocks, "-", unlist(lapply(c("R-NDNST", "R-NDST", "R-DNST", "R-DST"), function(x){1:table(blocks)[x]})))

set.seed(1)
saveRDS(data.frame(pid = pid, Group = randomizr::block_ra(blocks, conditions = c("SGLT-inhibitor", "Control"))), "data/random.RDS")
 