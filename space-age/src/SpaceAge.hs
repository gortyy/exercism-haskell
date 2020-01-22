module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

earthPeriod :: Float
earthPeriod = 31557600

round2dp :: Float -> Float
round2dp x = fromIntegral (round $ x * 100) / 100

earthSecondsRatio :: Planet -> Float
earthSecondsRatio Mercury = 0.2408467
earthSecondsRatio Venus   = 0.61519726
earthSecondsRatio Earth   = 1.0
earthSecondsRatio Mars    = 1.8808158
earthSecondsRatio Jupiter = 11.862615
earthSecondsRatio Saturn  = 29.447498
earthSecondsRatio Uranus  = 84.016846
earthSecondsRatio Neptune = 164.79132

ageOn :: Planet -> Float -> Float
ageOn planet seconds =
  round2dp $ seconds / (earthPeriod * earthSecondsRatio planet)
