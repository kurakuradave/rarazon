# rarazon
R, NLP, and Amazon Product Reviews

NOTE:
- Uses RSelenium package
- Requires Docker
- Requires Selenium Standalone Server Docker image (try selenium/standalone-firefox:2.53.0, port mapped to 4446 on host)

STEPS:
1. Start Docker Engine
2. Run image selenium/standalone-firefox:2.53.0
3. Call scrapProductReviesAmazon(), pass in the url to the product reviews page
4. Ensure that Product Review page url is obtained from the "See all xxx product reviews" link on amazon.com
5. See example code
