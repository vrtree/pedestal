#version 330 core

uniform vec3 uHand1;
uniform vec3 uHand2;
uniform vec3 uLight;

uniform mat4 uModelViewProjection;
uniform mat4 uModel;
uniform mat4 uInverseModel;
uniform vec3 uEye;

in vec3 aPosition;
in vec3 aNormal;
in vec2 aUV;
in vec3 aTangent;

out vec3 vPos;
out vec3 vHand1;
out vec3 vHand2;
out vec3 vNorm;
out vec3 vEye;

out vec3 vLight;

out vec2 vUV;


void main(){

  vUV   = aUV;
  vPos  = aPosition;
  vNorm = aNormal;


  vLight = ( uInverseModel * vec4( uLight  , 1. ) ).xyz;
  vEye   = ( uInverseModel * vec4( uEye    , 1. ) ).xyz;
  vHand1 = ( uInverseModel * vec4( uHand1  , 1. ) ).xyz;
  vHand2 = ( uInverseModel * vec4( uHand2  , 1. ) ).xyz;


  // Use this position to get the final position 
  gl_Position = uModelViewProjection * vec4( aPosition , 1.);

}