
# Setup -------------------------------------------------------------------

# Load required packages
library(unmarked)
library(tidyverse)
library(AICcmodavg)

# Import UMF
umf <- read_rds('data/processed/umf.rds')


# Null model --------------------------------------------------------------

m0 <-
  colext(
    # Psi, initial occupancy
    psiformula = ~1,
    # Gamma, colonization
    gammaformula = ~1,
    # Epsilon, extinction/persistence
    epsilonformula = ~1,
    # p, detection
    pformula = ~1,
    data = umf,
    method = 'BFGS')


# Model: detection p ------------------------------------------------------

# All detection variables included
m_p1 <-
    colext(
    # Psi, initial occupancy
    psiformula = ~1,
    # Gamma, colonization
    gammaformula = ~1,
    # Epsilon, extinction
    epsilonformula = ~1,
    # p, detection
    pformula = ~observer + disturbance + doy + wind + temperature,
    data = umf,
    method = 'BFGS')


# Observer, doy, temp
m_p2 <-
    colext(
    # Psi, initial occupancy
    psiformula = ~1,
    # Gamma, colonization
    gammaformula = ~1,
    # Epsilon, extinction
    epsilonformula = ~1,
    # p, detection
    pformula = ~observer + doy + temperature,
    data = umf,
    method = 'BFGS')


# Observer, disturbance, doy, wind
m_p3 <-
    colext(
    # Psi, initial occupancy
    psiformula = ~1,
    # Gamma, colonization
    gammaformula = ~1,
    # Epsilon, extinction
    epsilonformula = ~1,
    # p, detection
    pformula = ~observer + disturbance + doy + wind,
    data = umf,
    method = 'BFGS')


# Observer, disturbance
m_p4 <-
    colext(
    # Psi, initial occupancy
    psiformula = ~1,
    # Gamma, colonization
    gammaformula = ~1,
    # Epsilon, extinction
    epsilonformula = ~1,
    # p, detection
    pformula = ~observer + disturbance,
    data = umf,
    method = 'BFGS')


# Observer
m_p5 <-
    colext(
    # Psi, initial occupancy
    psiformula = ~1,
    # Gamma, colonization
    gammaformula = ~1,
    # Epsilon, extinction
    epsilonformula = ~1,
    # p, detection
    pformula = ~observer,
    data = umf,
    method = 'BFGS')


# Disturbance, wind
m_p6 <-
    colext(
    # Psi, initial occupancy
    psiformula = ~1,
    # Gamma, colonization
    gammaformula = ~1,
    # Epsilon, extinction
    epsilonformula = ~1,
    # p, detection
    pformula = ~disturbance + wind,
    data = umf,
    method = 'BFGS')


# Compare detection models ------------------------------------------------

aictab(
  cand.set = 
    list(
      Null = m0, 
      p_odywt = m_p1, 
      p_oyt = m_p2,
      p_odyw = m_p3,
      p_od = m_p4,
      p_o = m_p5,
      p_dw = m_p6))


# Model: occupancy psi ----------------------------------------------------

# All terms, full detection
m_p1_P1 <-
    colext(
    # Psi, initial occupancy
    psiformula = ~park + 
                  grs_1km + grs_5km + dvp_1km + dvp_5km +
                  shrub_mean + clin_max +
                  field_type + leased + harvest_limit,
    # Gamma, colonization
    gammaformula = ~1,
    # Epsilon, extinction
    epsilonformula = ~1,
    # p, detection
    pformula = ~observer + disturbance + doy + wind + temperature,
    data = umf,
    method = 'BFGS')


# Compare occupancy models ------------------------------------------------

aictab(
  cand.set = 
    list(
      Null = m0,
      p.all = m_p1,
      p.all_P.all = m_p1_P1))

