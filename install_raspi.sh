# install WiringPi manually
# postgres:
# - create user pixeml with passoword 'pixeml_nine_1_seven'
# - create database 'pixeml_tasks';
# - grant all privileges on all tables in schema public to pixeml;
# - grant create on schema public to pixeml;
apt install dune opam postgresql npm
npm install react
opam init
eval $(opam env)
opam install WiringPi batteries caqti caqti-driver-postgresql caqti-lwt dream lwt lwt_ppx ppx_yojson_conv
cd frontend/pixeml_frontend
npm run build
