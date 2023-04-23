library(ROpenWeatherMap)
library(tibble)

API_KEY = "4168e37e031c475c3ac014612bfc9564"

current_weather <- get_current_weather(API_KEY, city = "Hamburg")

weather_data <- tibble(weather.type = current_weather$weather$main,
                       weather.subtype = current_weather$weather$description,
                       weather.temp = current_weather$main$temp,
                       weather.feels_like = current_weather$main$feels_like,
                       weather.temp_min = current_weather$main$temp_min,
                       weather.temp_max = current_weather$main$temp_max,
                       weather.pressure = current_weather$main$pressure,
                       weather.humidity = current_weather$main$humidity,
                       wind.speed = current_weather$wind$speed,
                       wind.degree = current_weather$wind$deg)

weather_data
