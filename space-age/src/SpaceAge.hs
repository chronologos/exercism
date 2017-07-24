module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds =  earthYearToPlanetYear planet seconds/31557600

earthYearToPlanetYear :: Planet -> Float -> Float
earthYearToPlanetYear Earth x = x
earthYearToPlanetYear Mercury x = x/0.2408467
earthYearToPlanetYear Venus x = x/0.61519726
earthYearToPlanetYear Mars x = x/1.8808158
earthYearToPlanetYear Jupiter x = x/11.862615
earthYearToPlanetYear Saturn x = x/29.447498
earthYearToPlanetYear Uranus x = x/84.016846
earthYearToPlanetYear Neptune x = x/164.79132
