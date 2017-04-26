#include "vec3.h"

Vec3 operator+(const Vec3 &u, const Vec3 &v) {
    return Vec3(u.x + v.x, u.y + v.y, u.z + v.z);
}

Vec3 operator-(const Vec3 &u, const Vec3 &v) {
    return Vec3(u.x - v.x, u.y - v.y, u.z - v.z);
}

Vec3 operator*(float s, const Vec3 &v) {
    return Vec3(s * v.x, s * v.y, s * v.z);
}

Vec3 Vec3::normalize(const Vec3 &v) {
    float l = Vec3::length(v);

    if (l == 0.0) {
        return v;
    }

    return (1.0 / l) * v;
}

float Vec3::length(const Vec3 &v) {
    return sqrt(v.x * v.x + v.y * v.y + v.z * v.z);
}

Vec3 Vec3::reflect(const Vec3 &v, const Vec3 &n) {
    return v - (2.0 * Vec3::dot(v, n)) * n;
}

float Vec3::dot(const Vec3 &v1, const Vec3 &v2) {
    return v1.x * v2.x + v1.y * v2.y + v1.z * v2.z;
}

Vec3 Vec3::cross(const Vec3 &v1, const Vec3 &v2) {
    float x = v1.y * v2.z - v1.z * v2.y;
    float y = v1.z * v2.x - v1.x * v2.z;
    float z = v1.x * v2.y - v1.y * v2.x;
    return Vec3(x, y, z);
}

// http://mathworld.wolfram.com/DiskPointPicking.html
Vec3 Vec3::random_in_unit_disk() {
    float r = RAND(0.0, 1.0);     
    float theta = RAND(0.0, 2.0 * M_PI);
    return Vec3(r * cos(theta), r * sin(theta), 0.0);
}

// http://math.stackexchange.com/questions/87230/picking-random-points-in-the-volume-of-sphere-with-uniform-probability
Vec3 Vec3::random_in_unit_sphere() {
    float x = RAND(-1.0, 1.0);
    float y = RAND(-1.0, 1.0);
    float z = RAND(-1.0, 1.0);
    float u = RAND(0.0, 1.0);
    return pow(u, 1.0 / 3.0) * Vec3::normalize(Vec3(x, y, z));
}

Vec3 Vec3::clamp(const Vec3 &v, float min, float max) {
    Vec3 result = v;

    if (result.x < min) {
        result.x = min;
    }
    if (result.x > max) {
        result.x = max;
    }

    if (result.y < min) {
        result.y = min;
    }
    if (result.y > max) {
        result.y = max;
    }

    if (result.z < min) {
        result.z = min;
    }
    if (result.z > max) {
        result.z = max;
    }

    return result;
}

Vec3 Vec3::rotate_x(const Vec3 &v, float cos_theta, float sin_theta) {
    return Vec3(v.x, cos_theta * v.y - sin_theta * v.z, sin_theta * v.y + cos_theta * v.z);
}

Vec3 Vec3::rotate_y(const Vec3 &v, float cos_theta, float sin_theta) {
    return Vec3(cos_theta * v.x + sin_theta * v.z, v.y, -sin_theta * v.x + cos_theta * v.z);
}

Vec3 Vec3::rotate_z(const Vec3 &v, float cos_theta, float sin_theta) {
    return Vec3(cos_theta * v.x - sin_theta * v.y, sin_theta * v.x + cos_theta * v.y, v.z);
}
