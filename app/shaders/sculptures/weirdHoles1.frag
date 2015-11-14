#version 330 core


uniform vec3 uDimensions;

const float MAX_TRACE_DISTANCE = 50.;           // max trace distance
const float INTERSECTION_PRECISION = 0.000001;        // precision of the intersection
const int NUM_OF_TRACE_STEPS = 100;
const float PI  = 3.14159;

in vec3 vPos;
in vec3 vEye;
in vec3 vNorm;
in vec3 vLight;

in vec3 vHand1;
in vec3 vHand2;

in vec2 vUV;

out vec4 color;


float sdSphere( vec3 p, float s ){
  return length(p)-s;
}

float opRepSphere( vec3 p, vec3 c , float r)
{
    vec3 q = mod(p,c)-0.5*c;
    return sdSphere( q  , r );
}


float opS( float d1, float d2 )
{
    return max(-d1,d2);
}

vec2 opS( vec2 d1, vec2 d2 )
{
    return  -d1.x > d2.x ? d1 : d2;
}

float sdBox( vec3 p, vec3 b )
{
  vec3 d = abs(p) - b;
  return min(max(d.x,max(d.y,d.z)),0.0) +
         length(max(d,0.0));
}

float hash( float n ) { return fract(sin(n)*753.5453123); }
float noise( in vec3 x )
{
    vec3 p = floor(x);
    vec3 f = fract(x);
    f = f*f*(3.0-2.0*f);
  
    float n = p.x + p.y*157.0 + 113.0*p.z;
    return mix(mix(mix( hash(n+  0.0), hash(n+  1.0),f.x),
                   mix( hash(n+157.0), hash(n+158.0),f.x),f.y),
               mix(mix( hash(n+113.0), hash(n+114.0),f.x),
                   mix( hash(n+270.0), hash(n+271.0),f.x),f.y),f.z);
}



float fNoise( vec3 p ){
   
  float n;
  
  n += noise( p * 20. ) * .5;
  n += noise( p * 200. ) * .1;
  n += noise( p * 60. ) * .3;
  n += noise( p * 5. );
  
  return n;
 
}


//--------------------------------
// Modelling 
//--------------------------------
vec2 map( vec3 pos ){


  vec2 res = vec2( opRepSphere( pos , vec3( .2 * uDimensions * 2.3 ) , .04 ) , 1. );

  res = opS( res , vec2( sdBox( pos , vec3( .3 ) * uDimensions * 2.) , 2. )) ;

  return res;

}

vec2 mapN( vec3 pos ){

  vec2 res = map( pos );
  res.x -= .002 * fNoise( pos * 10. );

  return res;

}


vec2 calcIntersection( in vec3 ro, in vec3 rd ){

    float h =  INTERSECTION_PRECISION * 2.0;
    float t = 0.0;
    float res = -1.0;
    float id = -1.;
    
    for( int i=0; i < NUM_OF_TRACE_STEPS ; i++ ){
        
        if( h < INTERSECTION_PRECISION || t > MAX_TRACE_DISTANCE ) break;
        vec2 m = map( ro+rd*t );
        h = m.x;
        t += h;
        id = m.y;
        
    }

    if( t < MAX_TRACE_DISTANCE ) res = t;
    if( t > MAX_TRACE_DISTANCE ) id =-1.0;
    
    return vec2( res , id );
     
}

// Calculates the normal by taking a very small distance,
// remapping the function, and getting normal for that
vec3 calcNormal( in vec3 pos ){
    
  vec3 eps = vec3( 0.001, 0.0, 0.0 );
  vec3 nor = vec3(
      map(pos+eps.xyy).x - map(pos-eps.xyy).x,
      map(pos+eps.yxy).x - map(pos-eps.yxy).x,
      map(pos+eps.yyx).x - map(pos-eps.yyx).x );

  return normalize(nor);

}

// Calculates the normal by taking a very small distance,
// remapping the function, and getting normal for that
vec3 calcNoiseNormal( in vec3 pos ){
    
  vec3 eps = vec3( 0.001, 0.0, 0.0 );
  vec3 nor = vec3(
      mapN(pos+eps.xyy).x - mapN(pos-eps.xyy).x,
      mapN(pos+eps.yxy).x - mapN(pos-eps.yxy).x,
      mapN(pos+eps.yyx).x - mapN(pos-eps.yyx).x );

  return normalize(nor);
}

float calcAO( in vec3 pos, in vec3 nor )
{
  float occ = 0.0;
    float sca = 1.0;
    for( int i=0; i<5; i++ )
    {
        float hr = 0.01 + 0.612*float(i)/4.0;
        vec3 aopos =  nor * hr + pos;
        float dd = map( aopos ).x;
        occ += -(dd-hr)*sca;
        sca *= 0.5;
    }
    return clamp( 1.0 - 3.0*occ, 0.0, 1.0 );    
}


vec3 doColor( float id , float yVal , float lamb, float spec, float fresnel , float ao  ){

  vec3 color = vec3( 0. );

  vec3 gold = pow( spec , 20. ) * vec3( 1. , .5 , 0.);
  gold += lamb * vec3( .8 , .3 , .1 );
  gold += vec3( .1 , .0 , .0 );
  gold += (1. - fresnel) * vec3( .2 , .2, 0.);
  gold += ao * vec3( 1., 1.,1.);


  vec3 black = fresnel* vec3( .1 );
  black = mix( black , gold , uDimensions.x - yVal);

  // box
  if( id <= 1.8){

    color = black ;

  // rep1
  }else if( id > 1.8  && id <= 2. ){

    color = mix( black , gold , (id - 1.8 )* 5.);


  // udder
  }else if( id >  1.5 ){

    color = gold;

  // rep2
  }

  return color;


}

void main(){

  vec3 ro = vPos;
  vec3 rd = normalize( vPos - vEye );

  vec3 handDir1 = normalize( vHand1 - ro);
  vec3 handDir2 = normalize( vHand2 - ro);

  vec2 res = calcIntersection( ro , rd );


  vec3 col = vec3( 0. );
  if( res.y > .5 ){

    vec3 pos = ro + rd * res.x;

    vec3 handDir1 = normalize( vHand1 - pos);
    vec3 handDir2 = normalize( vHand2 - pos);

    vec3 norm = calcNoiseNormal( pos );
    float ao = calcAO( pos , norm );

    vec3 lightDir = normalize( vLight - pos );
    vec3 refl = reflect( lightDir , norm );
    float lamb = max( 0. , dot( lightDir , norm ) );
    float spec = max( 0. , dot( refl , rd ) );
    float fre  = max( 0. , dot( rd , -norm ) );

    col = doColor( res.y, pos.y , lamb, spec, fre , ao );

    ///col = norm * .5 + .5;

  }


  if( abs(vUV.x - .5) > .49 ||  abs(vUV.y - .5) > .49 ){
    col = vec3(1.);
  }


  color = vec4( col , 1. );



}